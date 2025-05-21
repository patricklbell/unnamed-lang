#pragma once

#include "ast.hpp"
#include "jit.hpp"
#include "type_generation.hpp"

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <unordered_map>

struct NamedValue {
  llvm::AllocaInst* llvm_alloca_inst = nullptr;
};

struct ModuleContext {
  std::string name;

  std::unique_ptr<llvm::Module> llvm_module;
};

struct CompilerContext {
  std::unique_ptr<LangJIT> llvm_jit;
  std::unique_ptr<llvm::LLVMContext> llvm_context;
  std::unique_ptr<llvm::IRBuilder<>> llvm_builder;

  std::unordered_map<std::string, ModuleContext> modules;

  CompilerContext();
};

void codegen(AST& ast, CompilerContext& context, TypeInfo& types);

int emit_object_code(CompilerContext& context);
