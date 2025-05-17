#include "code_generation.hpp"
#include "ast.hpp"
#include "logging.hpp"

#include <iostream>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>

#include <memory>
#include <unordered_map>

struct CGContext {
  int& nodei;
  int& last_visited_nodei;
  AST* ast;

  llvm::LLVMContext* llvm_context;
  llvm::IRBuilder<>* llvm_builder;
  llvm::Module* llvm_module;
  llvm::Function* llvm_function;

  std::unordered_map<std::string, ASTPrototypeData*> function_protos;
  std::unordered_map<std::string, NamedValue> named_values;
};

void visit_subtree(AST& ast, CGContext& ctx, int size);

// Helpers
static llvm::Function* get_function(std::string name, const CGContext& ctx) {
  // see if the function has already been added to the current module
  if (auto *F = ctx.llvm_module->getFunction(name))
    return F;

  // otherwise search our own reference
  auto FI = ctx.function_protos.find(name);
  if (FI != ctx.function_protos.end())
    return FI->second->llvm_function;

  return nullptr;
}

static llvm::AllocaInst* add_alloca_to_entry_block(llvm::Function* llvm_function, llvm::LLVMContext* llvm_context, std::string_view name) {
  llvm::IRBuilder<> tmp_llvm_builder(&llvm_function->getEntryBlock(), llvm_function->getEntryBlock().begin());
  // @todo types
  return tmp_llvm_builder.CreateAlloca(llvm::Type::getFloatTy(*llvm_context), nullptr, name);
}

static void visit_td_prototype(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Prototype);
  ASTPrototypeData* data = node.cast_data<ASTPrototypeData>();

  // make the function type:  double(double,double...)
  // @todo types
  std::vector<llvm::Type *> args_types(data->args.size(), llvm::Type::getFloatTy(*ctx.llvm_context));
  llvm::FunctionType *function_type = llvm::FunctionType::get(
    llvm::Type::getFloatTy(*ctx.llvm_context),
    args_types,
    false
  );

  data->llvm_function = llvm::Function::Create(
    function_type,
    llvm::Function::ExternalLinkage,
    data->name,
    ctx.llvm_module
  );

  unsigned idx = 0;
  for (auto &llvm_arg : data->llvm_function->args()) {
    llvm_arg.setName(data->args[idx].name);

    idx++;
  }

  // record the prototype @note type checking may already create a similar structure
  ctx.function_protos[data->name] = data;
}

static void visit_td_block(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Block);
  ASTBlockData* data = node.cast_data<ASTBlockData>();

  LANG_ASSERT(ctx.llvm_function != nullptr, "Block must exist inside a function");

  if (data->llvm_bb == nullptr)
    data->llvm_bb = llvm::BasicBlock::Create(*ctx.llvm_context);
  if (data->llvm_bb->getParent() == nullptr)
    data->llvm_bb->insertInto(ctx.llvm_function);

  data->llvm_bb->setName(data->llvm_bb_name);
  ctx.llvm_builder->SetInsertPoint(data->llvm_bb);
}

static void visit_td_if(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::If);
  LANG_ASSERT(node.num_children >= 2, "If must have at least 2 children (cond, block)");

  ASTIfData* data = node.cast_data<ASTIfData>();

  llvm::BasicBlock *llvm_merged_bb = llvm::BasicBlock::Create(*ctx.llvm_context, "ifcont");
  llvm::BasicBlock *llvm_else_bb = llvm_merged_bb;

  if (data->has_else) {
    ASTNode& else_block = ctx.ast->nodes[last_child(*ctx.ast, node.id)];
    LANG_ASSERT(else_block.type == ASTNodeType::Block);

    ASTBlockData* else_data = else_block.cast_data<ASTBlockData>();

    else_data->llvm_bb = llvm::BasicBlock::Create(*ctx.llvm_context);
    llvm_else_bb = else_data->llvm_bb;
  }

  // process each if-else [expression, block, expression, block ...]
  // and generate conditional jumps
  for (int i = 0, offset = node.id + 1; i + 1 < node.num_children; i+=2) {
    ASTNode& cond = ctx.ast->nodes[offset];
    ASTNode& block = ctx.ast->nodes[cond.id + cond.size];
    offset = block.id + block.size;

    LANG_ASSERT(block.type == ASTNodeType::Block);

    // @note recursion limit
    // generate the condition llvm value
    ctx.nodei = cond.id;
    visit_subtree(*ctx.ast, ctx, cond.size);

    ASTExpressionData* cond_data = cond.cast_data<ASTExpressionData>();
    LANG_ASSERT(cond_data->llvm_value != nullptr);

    ASTBlockData* block_data = block.cast_data<ASTBlockData>();
    block_data->llvm_bb = llvm::BasicBlock::Create(*ctx.llvm_context);

    bool last_condition = i + 3 >= node.num_children;

    if (last_condition) {
      ctx.llvm_builder->CreateCondBr(cond_data->llvm_value, block_data->llvm_bb, llvm_else_bb);
    } else {
      llvm::BasicBlock* next_cond_bb = llvm::BasicBlock::Create(*ctx.llvm_context, "cond", ctx.llvm_function);
      ctx.llvm_builder->CreateCondBr(cond_data->llvm_value, block_data->llvm_bb, next_cond_bb);
      ctx.llvm_builder->SetInsertPoint(next_cond_bb);
    }
  }

  // actually generate each block
  int offset = node.id + 1;
  for (int i = 0; i < node.num_children - 1; i+=2) {
    ASTNode& cond = ctx.ast->nodes[offset];
    ASTNode& block = ctx.ast->nodes[cond.id + cond.size];
    offset = block.id + block.size;

    LANG_ASSERT(block.type == ASTNodeType::Block);

    // @note recursion limit
    ctx.nodei = block.id;
    visit_subtree(*ctx.ast, ctx, block.size);

    ASTBlockData* block_data = block.cast_data<ASTBlockData>();
    if (block_data->llvm_bb->getTerminator() == nullptr)
      ctx.llvm_builder->CreateBr(llvm_merged_bb);
  }

  if (data->has_else) {
    ASTNode& else_block = ctx.ast->nodes[offset];
    LANG_ASSERT(else_block.type == ASTNodeType::Block);

    // @note recursion limit
    ctx.nodei = else_block.id;
    visit_subtree(*ctx.ast, ctx, else_block.size);

    ASTBlockData* block_data = else_block.cast_data<ASTBlockData>();
    if (block_data->llvm_bb->getTerminator() == nullptr)
      ctx.llvm_builder->CreateBr(llvm_merged_bb);
  }

  llvm_merged_bb->insertInto(ctx.llvm_function);
  ctx.llvm_builder->SetInsertPoint(llvm_merged_bb);
}

static void visit_td_while(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::While);
  LANG_ASSERT(node.num_children >= 2, "While must have at least 2 children (cond, block)");
  LANG_ASSERT(ctx.llvm_function != nullptr, "While must exist inside a function");

  ASTIfData* data = node.cast_data<ASTIfData>();

  ASTNode& cond = ctx.ast->nodes[node.id + 1];
  ASTNode& block = ctx.ast->nodes[cond.id + cond.size];

  LANG_ASSERT(block.type == ASTNodeType::Block);


  llvm::BasicBlock *llvm_cond_bb = llvm::BasicBlock::Create(*ctx.llvm_context, "whilecond");
  ctx.llvm_builder->CreateBr(llvm_cond_bb);
  llvm_cond_bb->insertInto(ctx.llvm_function);

  llvm::BasicBlock *llvm_merged_bb = llvm::BasicBlock::Create(*ctx.llvm_context, "whilecont");

  // @note recursion limit
  // generate the condition llvm value
  ctx.llvm_builder->SetInsertPoint(llvm_cond_bb);
  ctx.nodei = cond.id;
  visit_subtree(*ctx.ast, ctx, cond.size);

  ASTExpressionData* cond_data = cond.cast_data<ASTExpressionData>();
  LANG_ASSERT(cond_data->llvm_value != nullptr);

  ASTBlockData* block_data = block.cast_data<ASTBlockData>();
  block_data->llvm_bb = llvm::BasicBlock::Create(*ctx.llvm_context);

  ctx.llvm_builder->CreateCondBr(cond_data->llvm_value, block_data->llvm_bb, llvm_merged_bb);

  // generate loop body 
  // @note recursion limit
  ctx.nodei = block.id;
  visit_subtree(*ctx.ast, ctx, block.size);

  if (block_data->llvm_bb->getTerminator() == nullptr)
    ctx.llvm_builder->CreateBr(llvm_cond_bb);

  llvm_merged_bb->insertInto(ctx.llvm_function);
  ctx.llvm_builder->SetInsertPoint(llvm_merged_bb);
}

static void visit_td_function_definition(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::FunctionDefinition);

  ASTNode& prototype = ctx.ast->nodes[node.id + 1];
  ASTNode& body = ctx.ast->nodes[prototype.id + prototype.size];

  LANG_ASSERT((int)ASTFunctionDefinitionData::Children::Prototype == 1);
  LANG_ASSERT((int)ASTFunctionDefinitionData::Children::Body == 2);
  LANG_ASSERT(prototype.type == ASTNodeType::Prototype);
  LANG_ASSERT(body.type == ASTNodeType::Block);

  // visit the prototype immediately to generate llvm function
  ctx.nodei = prototype.id;
  visit_subtree(*ctx.ast, ctx, prototype.size);

  ASTPrototypeData* prototype_data = prototype.cast_data<ASTPrototypeData>();
  LANG_ASSERT(prototype_data->llvm_function != nullptr);

  ASTBlockData* body_data = body.cast_data<ASTBlockData>();

  // create basic block to insert alloca for arguments
  body_data->llvm_bb = llvm::BasicBlock::Create(*ctx.llvm_context);
  body_data->llvm_bb->insertInto(prototype_data->llvm_function);
  ctx.llvm_builder->SetInsertPoint(body_data->llvm_bb);

  unsigned idx = 0;
  for (auto &llvm_arg : prototype_data->llvm_function->args()) {
    auto &arg = prototype_data->args[idx];

    arg.llvm_alloca_inst = add_alloca_to_entry_block(prototype_data->llvm_function, ctx.llvm_context, arg.name);
    ctx.llvm_builder->CreateStore(&llvm_arg, arg.llvm_alloca_inst);
    ctx.named_values[arg.name] = NamedValue{ .llvm_alloca_inst = arg.llvm_alloca_inst };

    idx++;
  }

  // set context
  ctx.llvm_function = prototype_data->llvm_function;

  // continue to generate body 
  return;
}

static void visit_bu_function_definition(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::FunctionDefinition);

  // validate the generated code, checking for consistency.
  // @todo non-debug version for checking returns
  #ifdef LANG_DEBUG
  if (llvm::verifyFunction(*ctx.llvm_function, &llvm::errs()))
    ctx.llvm_function->eraseFromParent();
  #endif

  // clear context
  ctx.llvm_function = nullptr;
  ctx.named_values.clear();
}

static void visit_bu_variable(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Variable);
  ASTVariableData* data = node.cast_data<ASTVariableData>();

  auto named_value_lu = ctx.named_values.find(data->name);
  if (named_value_lu == ctx.named_values.end())
    // @todo logging, static checking
    return;

  data->llvm_value = ctx.llvm_builder->CreateLoad(llvm::Type::getFloatTy(*ctx.llvm_context), named_value_lu->second.llvm_alloca_inst);
}

static void visit_bu_assignment(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Assignment);
  ASTAssignmentData* data = node.cast_data<ASTAssignmentData>();

  ASTNode& exp = ctx.ast->nodes[node.id + 1];
  LANG_ASSERT((int)ASTAssignmentData::Children::Expression == 1);

  ASTExpressionData* exp_data = exp.cast_data<ASTExpressionData>();

  ctx.llvm_builder->CreateStore(exp_data->llvm_value, ctx.named_values[data->name].llvm_alloca_inst);

  data->llvm_value = exp_data->llvm_value;
}

static void visit_bu_literal(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Literal);
  ASTLiteralData* data = node.cast_data<ASTLiteralData>();

  data->llvm_value = llvm::ConstantFP::get(*ctx.llvm_context, llvm::APFloat(data->value));
}

static void visit_bu_binary_operator(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::BinaryOperator);
  ASTBinaryOperatorData* data = node.cast_data<ASTBinaryOperatorData>();

  ASTNode& LHS = ctx.ast->nodes[node.id + 1];
  ASTNode& RHS = ctx.ast->nodes[LHS.id + LHS.size];
  LANG_ASSERT((int)ASTBinaryOperatorData::Children::LHS == 1);
  LANG_ASSERT((int)ASTBinaryOperatorData::Children::RHS == 2);

  auto& LHS_llvm_value = LHS.cast_data<ASTExpressionData>()->llvm_value;
  auto& RHS_llvm_value = RHS.cast_data<ASTExpressionData>()->llvm_value;

  // @todo types
  switch (data->op) {
    case BinaryOperator::Plus:
      data->llvm_value = ctx.llvm_builder->CreateFAdd(LHS_llvm_value, RHS_llvm_value, "addtmp");
      break;
    case BinaryOperator::Minus:
      data->llvm_value = ctx.llvm_builder->CreateFSub(LHS_llvm_value, RHS_llvm_value, "subtmp");
      break;
    case BinaryOperator::Multiply:
      data->llvm_value = ctx.llvm_builder->CreateFMul(LHS_llvm_value, RHS_llvm_value, "multmp");
      break;
    case BinaryOperator::Divide:
      data->llvm_value = ctx.llvm_builder->CreateFDiv(LHS_llvm_value, RHS_llvm_value, "divtmp");
      break;
    case BinaryOperator::Less:
      data->llvm_value = ctx.llvm_builder->CreateFCmpULT(LHS_llvm_value, RHS_llvm_value, "ulttmp");
      break;
    case BinaryOperator::LessEq:
      data->llvm_value = ctx.llvm_builder->CreateFCmpULE(LHS_llvm_value, RHS_llvm_value, "uletmp");
      break;
    case BinaryOperator::Greater:
      data->llvm_value = ctx.llvm_builder->CreateFCmpUGT(LHS_llvm_value, RHS_llvm_value, "ugttmp");
      break;
    case BinaryOperator::GreaterEq:
      data->llvm_value = ctx.llvm_builder->CreateFCmpUGE(LHS_llvm_value, RHS_llvm_value, "ugetmp");
      break;
    case BinaryOperator::Equal:
      data->llvm_value = ctx.llvm_builder->CreateFCmpUEQ(LHS_llvm_value, RHS_llvm_value, "ueqtmp");
      break;
    case BinaryOperator::LogicalAnd:
      data->llvm_value = ctx.llvm_builder->CreateLogicalAnd(LHS_llvm_value, RHS_llvm_value, "ueqtmp");
      break;
    case BinaryOperator::LogicalOr:
      data->llvm_value = ctx.llvm_builder->CreateLogicalOr(LHS_llvm_value, RHS_llvm_value, "ueqtmp");
      break;
  }
}

static void visit_bu_unary_operator(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::UnaryOperator);
  ASTUnaryOperatorData* data = node.cast_data<ASTUnaryOperatorData>();

  ASTNode& exp = ctx.ast->nodes[node.id + 1];
  LANG_ASSERT((int)ASTUnaryOperatorData::Children::Expression == 1);

  auto& exp_llvm_value = exp.cast_data<ASTExpressionData>()->llvm_value;

  // @todo types
  switch (data->op) {
    case UnaryOperator::Minus:
      data->llvm_value = ctx.llvm_builder->CreateFNeg(exp_llvm_value);
      break;
    case UnaryOperator::LogicalNot:
      data->llvm_value = ctx.llvm_builder->CreateNeg(exp_llvm_value);
      break;
    case UnaryOperator::Plus:
      data->llvm_value = exp_llvm_value;
      break;
  }
}

static void visit_bu_call(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Call);
  ASTCallData* data = node.cast_data<ASTCallData>();

  // @todo type checking so these asserts are guaranteed

  // Look up the name in the global function table.
  llvm::Function *llvm_callee = get_function(data->name, ctx);
  LANG_ASSERT(llvm_callee != nullptr, "Function name could not be resolved.");
  LANG_ASSERT(llvm_callee->arg_size() == node.num_children, "Number of arguments did not match.");

  std::vector<llvm::Value *> llvm_args;

  for (auto& child : ASTChildrenIterator(*ctx.ast, node.id)) {
    llvm_args.push_back(child.cast_data<ASTExpressionData>()->llvm_value);
  }

  data->llvm_value = ctx.llvm_builder->CreateCall(llvm_callee, llvm_args, "calltmp");
}

static void visit_bu_return_statement(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Return);

  if (node.num_children == 0) {
    ctx.llvm_builder->CreateRetVoid();
  }

  ASTNode& expression = ctx.ast->nodes[node.id + 1];
  LANG_ASSERT((int)ASTReturnData::Children::Expression == 1);

  auto& llvm_value = expression.cast_data<ASTExpressionData>()->llvm_value;
  ctx.llvm_builder->CreateRet(llvm_value);
}

static void visit_bu_variable_definition(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::VariableDefinition);
  ASTVariableDefinitionData* data = node.cast_data<ASTVariableDefinitionData>();

  LANG_ASSERT(ctx.llvm_function != nullptr, "Variable definition must be inside a function.");
  
  auto nv = NamedValue{
    .llvm_alloca_inst = add_alloca_to_entry_block(ctx.llvm_function, ctx.llvm_context, data->name)
  };
  ctx.named_values[data->name] = nv;

  LANG_ASSERT(node.num_children == 1);
  LANG_ASSERT((int)ASTReturnData::Children::Expression == 1);
  ASTNode& exp = ctx.ast->nodes[node.id + 1];
  ASTExpressionData* exp_data = exp.cast_data<ASTExpressionData>();

  ctx.llvm_builder->CreateStore(exp_data->llvm_value, nv.llvm_alloca_inst);
}

static void visit_bu(ASTNode& node, CGContext& ctx) {
  switch (node.type) {
    case ASTNodeType::Literal:
      return visit_bu_literal(node, ctx);
    case ASTNodeType::Variable:
      return visit_bu_variable(node, ctx);
    case ASTNodeType::Assignment:
      return visit_bu_assignment(node, ctx);
    case ASTNodeType::BinaryOperator:
      return visit_bu_binary_operator(node, ctx);
    case ASTNodeType::UnaryOperator:
      return visit_bu_unary_operator(node, ctx);
    case ASTNodeType::Call:
      return visit_bu_call(node, ctx);
    case ASTNodeType::VariableDefinition:
      return visit_bu_variable_definition(node, ctx);
    case ASTNodeType::FunctionDefinition:
      return visit_bu_function_definition(node, ctx);
    case ASTNodeType::Return:
      return visit_bu_return_statement(node, ctx);
    case ASTNodeType::Block:
    case ASTNodeType::If:
    case ASTNodeType::ExpressionStatement:
    case ASTNodeType::While:
    case ASTNodeType::Module:
    case ASTNodeType::Prototype:
    case ASTNodeType::Unknown:
      return;
  }
}

static void visit_td(ASTNode& node, CGContext& ctx) {
  switch (node.type) {
    case ASTNodeType::Prototype:
      return visit_td_prototype(node, ctx);
    case ASTNodeType::FunctionDefinition:
      return visit_td_function_definition(node, ctx);
    case ASTNodeType::Block:
      return visit_td_block(node, ctx);
    case ASTNodeType::If:
      return visit_td_if(node, ctx);
    case ASTNodeType::While:
      return visit_td_while(node, ctx);
    case ASTNodeType::Module:
    case ASTNodeType::Return:
    case ASTNodeType::Assignment:
    case ASTNodeType::ExpressionStatement:
    case ASTNodeType::Literal:
    case ASTNodeType::Variable:
    case ASTNodeType::BinaryOperator:
    case ASTNodeType::UnaryOperator:
    case ASTNodeType::Call:
    case ASTNodeType::VariableDefinition:
    case ASTNodeType::Unknown:
      return;
  }
}

void visit_subtree(AST& ast, CGContext& ctx, int size) {
  // hybrid top to bottom then bottom to top, right to left traversal
  int endi = ctx.nodei + size;
  for (; ctx.nodei < endi && ctx.nodei >= 0;) {
    auto& node = ast.nodes[ctx.nodei];
    LANG_ASSERT(node.id == ctx.nodei);

    // if we have visited all the children, visit the node
    if (node.num_children == 0 || ctx.last_visited_nodei == last_child(ast, node.id)) {
      // mark this as the last visited node
      ctx.last_visited_nodei = node.id;

      if (node.parent < 0)
        break;

      // go to the next sibling or parent
      if (node.id != last_child(ast, node.parent)) {
        ctx.nodei += node.size;
      } else {
        ctx.nodei = node.parent;
      }

      // @note visit can modify nodei and last visited nodei
      if (node.num_children == 0)
        visit_td(node, ctx);
      visit_bu(node, ctx);

      continue;
    }

    // visit the subtree 
    ctx.nodei++;

    // @note visit can modify nodei and last visited nodei
    visit_td(node, ctx);
  }
}


#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putcharf - putchar that takes a float and returns 0.
extern "C" DLLEXPORT float putcharf(float X) {
  fputc((char)X, stderr);
  return 0;
}

CompilerContext::CompilerContext() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  llvm_context = std::make_unique<llvm::LLVMContext>();
  llvm_builder = std::make_unique<llvm::IRBuilder<>>(*llvm_context);

  auto jit_result = LangJIT::Create();
  if (auto e = jit_result.takeError()) {
    llvm::errs() << "Error creating jit: " << llvm::toString(std::move(e)) << "\n";
    return;
  }
  llvm_jit = std::move(*jit_result);
}

static void make_module(std::string name, CompilerContext& ctx) {
  auto llvm_module = std::make_unique<llvm::Module>(name, *ctx.llvm_context);

  ctx.modules[name] = ModuleContext{
    .name = name,
    .llvm_module = std::move(llvm_module),
  };
}

void codegen_module(AST& ast, CompilerContext& compiler_context, std::string name) {
  int nodei = 0, last_visited_nodei = -1;

  make_module(name, compiler_context);
  auto ctx = CGContext{
    .nodei = nodei,
    .last_visited_nodei = last_visited_nodei,
    .ast = &ast,
    .llvm_context = compiler_context.llvm_context.get(),
    .llvm_builder = compiler_context.llvm_builder.get(),
    .llvm_module = compiler_context.modules[name].llvm_module.get(),
  };

  if (ast.nodes.empty())
    return;

  visit_subtree(ast, ctx, ast.nodes.size());
}


int emit_object_code(CompilerContext& ctx) {
  auto target_triple = LLVMGetDefaultTargetTriple();

  std::string error;
  const llvm::Target* llvm_target = llvm::TargetRegistry::lookupTarget(target_triple, error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!llvm_target) {
    std::cerr << error;
    return 1;
  }

  // use generic cpu with no extra features @todo enable/disable features
  const char* cpu = "generic";
  const char* features = "generic";

  llvm::TargetOptions llvm_target_options;
  llvm::TargetMachine* llvm_target_machine = llvm_target->createTargetMachine(
    target_triple,
    /* cpu */ "generic",
    /* features */ "",
    llvm_target_options,
    llvm::Reloc::PIC_
  );
  auto DL = llvm_target_machine->createDataLayout();

  for (auto& mm : ctx.modules) {
    auto dest_path = mm.first + ".o";
    auto ir_dest_path = mm.first + ".ir";
    auto& module = mm.second;

    // dump IR
    std::error_code error_code;
    llvm::raw_fd_ostream ir_dest(ir_dest_path, error_code, llvm::sys::fs::OF_None);

    if (error_code) {
      std::cerr << "Could not open file: " << error_code.message() << std::endl;
      return 1;
    }

    module.llvm_module->print(ir_dest, nullptr);

    // dump obj
    module.llvm_module->setDataLayout(DL);
    module.llvm_module->setTargetTriple(target_triple);

    llvm::raw_fd_ostream dest(dest_path, error_code, llvm::sys::fs::OF_None);

    if (error_code) {
      std::cerr << "Could not open file: " << error_code.message() << std::endl;
      return 1;
    }

    llvm::legacy::PassManager pass;
    if (llvm_target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
      std::cerr << "Target machine can't emit a file of this type" << std::endl;
      return 1;
    }

    pass.run(*module.llvm_module);
    dest.flush();
  }

  return 0;
}

void jit_run_module(CompilerContext& ctx, std::string name) {
  auto module_lu = ctx.modules.find(name);
  if (module_lu == ctx.modules.end()) {
    std::cerr << "No module \"" << name << "\" found\n";
    return;
  }

  auto& module = module_lu->second;

  module.llvm_module->setDataLayout(ctx.llvm_jit->getDataLayout());

  // Create a ResourceTracker to track JIT'd memory allocated to our
  // anonymous expression -- that way we can free it after executing.
  auto RT = ctx.llvm_jit->getMainJITDylib().createResourceTracker();

  auto TSM = llvm::orc::ThreadSafeModule(
    std::move(module.llvm_module),
    std::move(ctx.llvm_context)
  );
  ctx.modules.erase(ctx.modules.find(name));

  if (auto e = ctx.llvm_jit->addModule(std::move(TSM), RT)) {
    std::cerr << llvm::toString(std::move(e)) << "\n";
    return;
  }

  // Search the JIT for the __anon_expr symbol.
  auto ExprSymbol = ctx.llvm_jit->lookup("main");
  if (auto e = ExprSymbol.takeError()) {
    std::cerr << llvm::toString(std::move(e)) << "\n";
    return;
  }

  // Get the symbol's address and cast it to the right type (takes no
  // arguments, returns a float) so we can call it as a native function.
  float (*FP)() = ExprSymbol->getAddress().toPtr<float (*)()>();
  std::cout << "Evaluated to " << FP() << "\n";

  // Delete the anonymous expression module from the JIT.
  if (auto e = RT->remove()) {
    std::cerr << llvm::toString(std::move(e)) << "\n";
  }
}
