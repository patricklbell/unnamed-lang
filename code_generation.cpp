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
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>

#include <memory>
#include <unordered_map>

struct CGContext {
  AST* ast;

  llvm::LLVMContext* llvm_context;
  llvm::IRBuilder<>* llvm_builder;
  llvm::Module* llvm_module;
  llvm::Function* llvm_function = nullptr;

  std::unordered_map<std::string, ASTPrototypeData*> function_protos;
  std::unordered_map<std::string, NamedValue> named_values;
};

// Helpers
// @todo might be better to bake this into the structure
// @note returns sibling if there are no children
static int last_child(AST& ast, int nodei) {
  int child_id = nodei + 1;

  int num_children = ast.nodes[nodei].num_children;
  for (int i = 0; i < num_children - 1; ++i) {
    child_id += ast.nodes[child_id].size;
  }

  return child_id;
}

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

static void visit_td_prototype(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Prototype);
  ASTPrototypeData* data = node.cast_data<ASTPrototypeData>();

  // make the function type:  double(double,double...)
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

  // only store parameter named values and set context if we are in a definition (not extern) 
  bool has_definition = ctx.ast->nodes[node.parent].type == ASTNodeType::FunctionDefinition;

  unsigned idx = 0;
  for (auto &f_arg : data->llvm_function->args()) {
    auto& name = data->args[idx++].name;
    f_arg.setName(name);
    if (has_definition)
      ctx.named_values[name] = NamedValue{ .llvm_value = &f_arg };
  }
  if (has_definition)
    ctx.llvm_function = data->llvm_function;

  // record the prototype @note type checking may already create a similar structure
  ctx.function_protos[data->name] = data;
}

static void visit_td_block(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(ctx.llvm_function != nullptr, "Block cannot exist outside a function.");

  // create a new basic block to start insertion into.
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(*ctx.llvm_context, "block", ctx.llvm_function);
  ctx.llvm_builder->SetInsertPoint(BB);
}

static void visit_bu_function_definition(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::FunctionDefinition);

  ASTNode& prototype = ctx.ast->nodes[node.id + 1];
  ASTNode& body = ctx.ast->nodes[prototype.id + prototype.size];

  LANG_ASSERT((int)ASTFunctionDefinitionData::Children::Prototype == 1);
  LANG_ASSERT((int)ASTFunctionDefinitionData::Children::Body == 2);
  LANG_ASSERT(prototype.type == ASTNodeType::Prototype);
  LANG_ASSERT(body.type == ASTNodeType::Block);
  LANG_ASSERT(ctx.llvm_function == prototype.cast_data<ASTPrototypeData>()->llvm_function);

  llvm::Function* llvm_function = ctx.llvm_function;
  ctx.named_values.clear();
  ctx.llvm_function = nullptr;

  if (body.num_children > 0) {
    // @temp assume last statement of block is a return
    ASTNode& ret = ctx.ast->nodes[last_child(*ctx.ast, body.id)];

    LANG_ASSERT(ret.type == ASTNodeType::Return);
    LANG_ASSERT((int)ASTReturnData::Children::Expression == 1);
    ASTNode& expr = ctx.ast->nodes[ret.id + 1];

    ASTExpressionData* expr_data = expr.cast_data<ASTExpressionData>();
    ctx.llvm_builder->CreateRet(expr_data->llvm_value);

    // validate the generated code, checking for consistency.
    #ifdef LANG_DEBUG
    llvm::verifyFunction(*llvm_function, &llvm::errs());
    #endif

    return;
  }

  // error reading body, remove function.
  llvm_function->eraseFromParent();
  return;
}

static void visit_bu_literal(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Literal);
  ASTLiteralData* data = node.cast_data<ASTLiteralData>();

  data->llvm_value = llvm::ConstantFP::get(*ctx.llvm_context, llvm::APFloat(data->value));
}

static void visit_bu_call(ASTNode& node, CGContext& ctx) {
  LANG_ASSERT(node.type == ASTNodeType::Call);
  ASTCallData* data = node.cast_data<ASTCallData>();

  // @todo type checking so these asserts are guaranteed

  // Look up the name in the global module table.
  llvm::Function *llvm_callee = get_function(data->name, ctx);
  LANG_ASSERT(llvm_callee != nullptr, "Function name could not be resolved.");
  LANG_ASSERT(llvm_callee->arg_size() == node.num_children, "Number of arguments did not match.");

  std::vector<llvm::Value *> llvm_args;
  // @todo custom iterator for children?
  for (int childi = node.id + 1, i = 0; i < node.num_children; ++i) {
    auto& child = ctx.ast->nodes[childi];
    childi += child.size;

    llvm_args.push_back(child.cast_data<ASTExpressionData>()->llvm_value);
  }

  data->llvm_value = ctx.llvm_builder->CreateCall(llvm_callee, llvm_args, "calltmp");
}

static void visit_bu(ASTNode& node, CGContext& ctx) {
  switch (node.type) {
    case ASTNodeType::Literal:
      return visit_bu_literal(node, ctx);
    case ASTNodeType::Call:
      return visit_bu_call(node, ctx);
    case ASTNodeType::FunctionDefinition:
      return visit_bu_function_definition(node, ctx);
    case ASTNodeType::Module:
    case ASTNodeType::Prototype:
    case ASTNodeType::Block:
    case ASTNodeType::Assignment:
    case ASTNodeType::Return:
    case ASTNodeType::Unknown:
      return;
  }
}

static void visit_td(ASTNode& node, CGContext& ctx) {
  switch (node.type) {
    case ASTNodeType::Prototype:
      return visit_td_prototype(node, ctx);
    case ASTNodeType::Block:
      return visit_td_block(node, ctx);
    case ASTNodeType::Module:
    case ASTNodeType::FunctionDefinition:
    case ASTNodeType::Assignment:
    case ASTNodeType::Return:
    case ASTNodeType::Literal:
    case ASTNodeType::Call:
    case ASTNodeType::Unknown:
      return;
  }
}


#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT float putchard(float X) {
  fputc((char)X, stderr);
  return 0;
}

CompilerContext::CompilerContext() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmParser();
  llvm::InitializeNativeTargetAsmPrinter();

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
  llvm_module->setDataLayout(ctx.llvm_jit->getDataLayout());

  ctx.modules[name] = ModuleContext{
    .name = name,
    .llvm_module = std::move(llvm_module),
  };
}

void codegen_module(AST& ast, CompilerContext& compiler_context, std::string name) {
  make_module(name, compiler_context);
  auto ctx = CGContext{
    .ast = &ast,
    .llvm_context = compiler_context.llvm_context.get(),
    .llvm_builder = compiler_context.llvm_builder.get(),
    .llvm_module = compiler_context.modules[name].llvm_module.get(),
  };

  if (ast.nodes.empty())
    return;

  // hybrid top to bottom then bottom to top, right to left traversal
  int last_visited_node = -1;
  int nodei;
  for (nodei = 0; nodei < ast.nodes.size() && nodei >= 0;) {
    auto& node = ast.nodes[nodei];
    LANG_ASSERT(node.id == nodei);
    LANG_ASSERT((node.parent == -1 && node.id == 0) || (node.parent < ast.nodes.size() && node.parent >= 0));

    // if we have visited all the children, visit the node
    if (node.num_children == 0 || last_visited_node == last_child(ast, node.id)) {
      if (node.num_children == 0)
        visit_td(node, ctx);
      visit_bu(node, ctx);

      // mark this as the last visited node
      last_visited_node = node.id;

      if (node.parent < 0)
        break;

      // go to the next sibling or parent
      if (node.id != last_child(ast, node.parent)) {
        nodei += node.size;
      } else {
        nodei = node.parent;
      }

      continue;
    }

    // visit the subtree 
    visit_td(node, ctx);
    nodei++;
  }

  // make sure we finished back at the root node
  LANG_ASSERT(nodei == 0);
}

void run_module(CompilerContext& ctx, std::string name) {
  if (ctx.modules.find(name) == ctx.modules.end()) {
    std::cerr << "No module \"" << name << "\" found\n";
    return;
  }

  auto& module = ctx.modules[name];

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
