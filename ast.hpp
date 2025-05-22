#pragma once

#include "lexer.hpp"
#include "typing.hpp"
#include "logging.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <memory>

enum class ASTNodeType : char {
  Unknown = 0,
  Return,
  VariableDefinition,
  ExpressionStatement,
  If,
  While,
  Prototype,
  FunctionDefinition,
  Block,
  Module,

  IntLiteral,
  FloatLiteral,
  Variable,
  BinaryOperator,
  UnaryOperator,
  Call,
  Assignment,
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

struct ASTUnkownData {};

struct ASTExpressionData {
  llvm::Value* llvm_value = nullptr;
  Type* resolved_type = nullptr;
};

struct ASTExpressionStatementData {};

struct ASTIntLiteralData : ASTExpressionData {
  int value;
};

struct ASTFloatLiteralData : ASTExpressionData {
  float value;
};

struct ASTVariableData: ASTExpressionData {
  std::string name;
};

enum class BinaryOperator {
  Plus,
  Minus,
  Multiply,
  Divide,
  Less,
  LessEq,
  Greater,
  GreaterEq,
  Equal,
  LogicalAnd,
  LogicalOr,
};

std::string to_string(BinaryOperator op);

struct ASTBinaryOperatorData : ASTExpressionData {
  BinaryOperator op;

  enum class Children {
    LHS = 1,
    RHS,
  };
};

enum class UnaryOperator {
  Plus,
  Minus,
  LogicalNot,
};

std::string to_string(UnaryOperator op);

struct ASTUnaryOperatorData : ASTExpressionData {
   UnaryOperator op;

  enum class Children {
    Expression = 1,
  };
};

struct ASTCallData : ASTExpressionData {
  std::string name;
};

struct ASTReturnData {
  enum class Children {
    Expression = 1,
  };
};

struct ASTAssignmentData : ASTExpressionData {
  std::string name;

  enum class Children {
    Expression = 1,
  };
};

struct ASTVariableDefinitionData {
  std::string name;
  std::string type = ""; // @optional
  Type* resolved_type = nullptr;

  enum class Children {
    Expression = 1,
  };
};

struct ASTIfData {
  bool has_else;

  llvm::BasicBlock* llvm_merged_bb = nullptr;
};

struct ASTWhileData {};

struct ArgumentPrototype {
  std::string name;
  std::string type;
  Type* resolved_type = nullptr;
  llvm::AllocaInst* llvm_alloca_inst = nullptr;
};

struct ASTPrototypeData {
  std::string name;
  std::vector<ArgumentPrototype> args;
  std::string return_type;
  Type* resolved_return_type = nullptr;

  llvm::Function* llvm_function = nullptr;
};

struct ASTBlockData {
  std::string llvm_bb_name = "block";
  llvm::BasicBlock* llvm_bb = nullptr;
  bool dont_create_basic_block = false;
};

struct ASTModuleData {
  std::string name;
};

struct ASTFunctionDefinitionData {
  enum class Children {
    Prototype = 1,
    Body,
  };
};

struct AST;

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

  template<typename T>
  const T* cast_data() const {
    return (const T*)data.get();
  }
};

std::string to_string(const ASTNode& node);

struct AST {
  std::vector<ASTNode> nodes;

  ASTNode& make_node(ASTNodeType type);
};


class ASTChildrenIterator {
private:
  struct ASTChildIter {
    ASTChildIter(AST& ast, int nodei, int child_index) : ast(ast), id(nodei + 1), child_index(child_index) { };
    ASTChildIter operator++() {
      id += ast.nodes[id].size;
      child_index++;
      return *this;
    }
    ASTNode& operator*() {
      return ast.nodes[id];
    }
    bool operator!=(ASTChildIter &cmp_childiter) {
      return child_index != cmp_childiter.child_index;
    }
    AST& ast;
    int id;
    int child_index;
  };

public:
  ASTChildrenIterator(AST& ast, int nodei) : ast(ast), nodei(nodei) {}

  ASTChildIter begin() { return ASTChildIter(ast, nodei, 0); }

  ASTChildIter end() { return ASTChildIter(ast, nodei, ast.nodes[nodei].num_children); }

private:
  AST& ast;
  int nodei;
};

int last_child(AST& ast, int nodei);

void parse_module_ast(AST& ast, std::string name, Lexer& lexer, Logger& logger);

void print_ast(const AST& ast);
