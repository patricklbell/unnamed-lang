#pragma once

#include "ast.hpp"
#include "jit.hpp"

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <unordered_map>

struct NamedValue {
  llvm::Value* llvm_value = nullptr;
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

void codegen_module(AST& ast, CompilerContext& context, std::string name);

void run_module(CompilerContext& context, std::string name);
