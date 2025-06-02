#pragma once

#include "typing.hpp"
#include "files.hpp"
#include "logging.hpp"
#include "helpers.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>

// ast nodes
enum class ASTNodeType : char {
  Unknown = 0,

  Module,

  TypeDefinition,
  GenericParameterDefinitionList,
  GenericParameterDefinition,
  TypeAlias,
  GenericParameterList,
  FunctionDefinition,
  ParameterDefinitionList,
  ParameterDefinition,
  StructDefinition,
  StructEntry,
  EnumDefinition,
  EnumEntry,
  UnionDefinition,
  UnionEntry,
  
  IntLiteral,
  FloatLiteral,
  StringLiteral,
  UndefinedLiteral,
  StructLiteral,
  StructLiteralEntryList,
  StructLiteralEntry,
  Variable,
  BinaryOperator,
  UnaryOperator,
  CastOperator,
  Call,

  FunctionDeclaration,
  VariableDeclaration,
  Return,
  ExpressionStatement,
  If,
  While,
  Block,
};

std::string to_string(const ASTNodeType& type);

// a module is the top-level node and contains
// a list of type definition, variable declarations
// and function declarations.
enum class ModuleChildren {
  TypeDefinitions = -1,
  VariableDeclaration = -1,
  FunctionDeclaration = -1,
};

//
// types
//
// defines a new type with a certain name.
// types can be generic, meaning they accept a
// a list of parameters. their parameters can 
// be meta-types (set of types) or types (eg. int).
// when the type is realised these parameters are
// inferred or explicitly provided, in either case
// all parameters must be resolved at compile time.
// @note the list of generic parameters may be
// missing.
struct ASTTypeDefinitionData {
  bool is_generic;
};

enum class TypeDefinitionChildren {
  GenericParameterDefinitionList  = 1,
  Type                            = 2,
};

enum class GenericParameterDefinitionListChildren {
  GenericParameterDefinition = -1,
};

// defines a parameterisation of a generic type,
// if the type is ommitted it is assumed to be 
// any type (eg. meta-type "Any"). unlike function
// parameters since a parameter can be a meta-type the
// default value can be a type.
struct ASTGenericParameterDefinitionData {
  bool has_type;
  bool has_default_value;
};

// type or expression/type may be missing
enum class GenericParameterDefinitionChildren {
  Type              = 1,
  ExpressionOrType  = 2,
};

// @todo meta types
// a realisation of a type. there are two types:
//  - derivative, meaning this is a realisation of 
//    generic type or an alias.
//  - definition, this defines a new type.
struct ASTType {
  Type* resolved_type = nullptr;
};

struct ASTTypeAliasData : ASTType {
  bool has_arguments;
};

// type argument list may be missing
enum class TypeAliasChildren {
  GenericParameterList = 1,
};

// @note child could be either an a type or a compile time expression
enum class GenericParameterListChildren {
  Type = -1,
  Expression = -1,
};

// defines a function, if the function is missing
// it's body then the resulting type is a 
// function type, otherwise the type is the function
// itself (ie. not the same as a function with the same prototype).
struct ASTFunctionDefinitionData : ASTType {
  // void is not a type in itself, but rather part
  // of the function type
  bool is_void;
};

// functions
enum class FunctionDefinitionChildren {
  ParameterDefinitionList = 1,
  Type                    = 2,
};

enum class ParameterDefinitionListChildren {
  ParameterDefinition = -1,
};

struct ASTParameterDefinitionData {
  llvm::AllocaInst* llvm_alloca_inst = nullptr;
  
  bool has_default_value;
};

// @note expression may be missing
enum class ParameterDefinitionChildren {
  Type = 1,
  Expression = 2,
};

// structs
struct ASTStructDefinitionData : ASTType {

};

// list of struct entries
enum class StructDefinitionChildren {
  StructEntry = -1,
};

struct ASTStructEntryData {
  bool is_spread;
  bool has_type;
  bool has_default_value;
};

// @note type or expression may be missing
enum class StructEntryChildren {
  Type = 1,
  Expression = 2,
};

// enums
struct ASTEnumDefinitionData : ASTType {
  bool has_type;
};

enum class EnumDefinitionChildren {
  Type = 1,
  EnumEntry = -1,
};

struct ASTEnumEntryData {
  bool is_spread;
  bool has_value;
};

// @note expression may not be present
enum class EnumEntryChildren {
  Type = 1,
  Expression = 1,
};

// unions
struct ASTUnionDefinitionData : ASTType {

};

enum class UnionDefinitionChildren {
  UnionEntry = -1,
};

struct ASTUnionEntryData {
  bool is_spread;
};

// @note type may be missing
enum class UnionEntryChildren {
  Type = 1,
};

//
// expressions
//
// common expression data which must be realised as
// an actual node type
struct ASTExpression {
  llvm::Value* llvm_value = nullptr;
  Type* resolved_type = nullptr;
};

struct ASTIntLiteralData : ASTExpression {
  int value;
};

struct ASTFloatLiteralData : ASTExpression {
  float value;
};

struct ASTStringLiteralData : ASTExpression {
  std::string value;
};

struct ASTStructLiteralData : ASTExpression {};

// @note any missing entries should be initialized following
// the struct's default values.
enum class StructLiteralChildren {
  Type = 1,
  StructLiteralEntryList = 2,
};

enum class StructLiteralEntryListChildren {
  StructLiteralEntry = -1,
};

// this node is needed to store the name 
// of the struct entry
enum class StructLiteralEntryChildren {
  Expression = 1,
};

enum class UnaryOperator {
  // prefix
  PrefixIncrement,
  PrefixDecrement,
  Plus,
  Minus,
  LogicalNot,
  Dereference,
  AddressOf,
  // postfix
  PostfixIncrement,
  PostfixDecrement,
  MemberAccess,
  PointerMemberAccess,
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
  NotEqual,
  LogicalAnd,
  LogicalOr,
  Assign,
  PlusAssign,
  MinusAssign,
  MultiplyAssign,
  DivideAssign,
  // postfix
  Subscript,
};

std::string to_string(BinaryOperator op);
std::string to_string(UnaryOperator op);

struct ASTBinaryOperatorData : ASTExpression {
  BinaryOperator op;
};

enum class BinaryOperatorChildren {
  LHSExpression = 1,
  RHSExpression = 2,
};

struct ASTUnaryOperatorData : ASTExpression {
  UnaryOperator op;
};

enum class UnaryOperatorChildren {
  Expression = 1,
};

struct ASTCastOperatorData : ASTExpression {};

enum class CastOperatorChildren {
  Type = 1,
  Expression = 2,
};

struct ASTCallData : ASTExpression {
  bool is_generic;
  bool is_function_pointer;
};

// @note generic parameter list or function pointer may be missing
enum class CallChildren {
  GenericParameterList = 1,
  FunctionPointer = 2,
  Expression = -1,
};

//
// other
//
struct ASTFunctionDeclarationData {
  llvm::Function* llvm_function = nullptr;

  bool is_generic;
  bool has_type;
};

// @note generic parameter definition list or type may be missing
enum class FunctionDeclarationChildren {
  GenericParameterDefinitionList = 1,
  Type = 2,
  Block = 3,
};

struct ASTVariableDeclarationData {
  bool has_type;
};

// type may be missing
enum class VariableDeclarationChildren {
  Type = 1,
  Expression = 2,
};

struct ASTIfData {
  bool has_else;

  llvm::BasicBlock* llvm_merged_bb = nullptr;
};

struct ASTBlockData {
  std::string llvm_bb_name = "block";
  llvm::BasicBlock* llvm_bb = nullptr;
  bool dont_create_basic_block = false;
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

  name_id name = -1;
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

std::string to_string(const AST& ast, const ASTNode& node);

struct AST {
  std::vector<ASTNode> nodes;
  std::unordered_multimap<name_id, std::string> name_id_to_name = {
    { name_id_int, "int" },
    { name_id_float, "float" },
    { name_id_ptr, "ptr" },
    { name_id_buffer, "buffer" },
  };
  name_id id_counter = 0;

  ASTNode& make_node(ASTNodeType type);

  std::string get_name(name_id id) const;
  name_id get_name_id(std::string name);
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

void construct_data(ASTNode& node);

int last_child(AST& ast, int nodei);

ASTNode& get_child(AST& ast, int nodei, int childi);

void print_ast(const AST& ast);

void add_child(AST& ast, int parent, int child);

int make_parent(AST& ast, ASTNodeType type, int child);
