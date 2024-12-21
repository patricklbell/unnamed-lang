#pragma once

#include "lexer.hpp"
#include "logging.hpp"
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <memory>

enum class ASTNodeType : char {
  Unknown = 0,
  Assignment,
  Return,
  Literal,
  Call,
  Prototype,
  FunctionDefinition,
  Block,
  Module,
};

std::string to_string(const ASTNodeType& type);

using unique_void_ptr = std::unique_ptr<void, void(*)(void const*)>;

template<typename T>
auto unique_void(T * ptr) -> unique_void_ptr
{
    return unique_void_ptr(ptr, [](void const * data) {
         T const * p = static_cast<T const*>(data);
         delete p;
    });
}

struct ASTUnkownData {

};

struct ASTExpressionData {
  llvm::Value* llvm_value = nullptr;
};

struct ASTLiteralData : ASTExpressionData {
  float value;
};

struct ASTCallData : ASTExpressionData {
  std::string name;
};

struct ASTReturnData {
  enum class Children {
    Expression = 1,
  };
};

struct ASTAssignmentData {
  std::string name;

  enum class Children {
    Expression = 1,
  };
};

struct ArgumentPrototype {
  std::string name;
};

struct ASTPrototypeData {
  std::string name;
  std::vector<ArgumentPrototype> args;

  llvm::Function* llvm_function = nullptr;
};

struct ASTBlockData {

};

struct ASTModuleData {

};

struct ASTFunctionDefinitionData {
  enum class Children {
    Prototype = 1,
    Body,
  };
};

struct ASTNode {
  TextSpan span;
  ASTNodeType type;

  int id = 0;
  int parent = -1;

  int size = 1;
  int num_children = 0;

  bool malformed = false;

  unique_void_ptr data;

  ASTNode() = delete;
  ASTNode(ASTNodeType type);

  template<typename T>
  T* cast_data() {
    return (T*)data.get();
  }
};

std::string to_string(const ASTNode& node);

struct AST {
  std::vector<ASTNode> nodes;

  ASTNode& make_node(ASTNodeType type);
};

void make_ast(AST& ast, Lexer& lexer, Logger& logger);

void print_ast(const AST& ast);
