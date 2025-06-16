#pragma once

#include "typing.hpp"
#include "files.hpp"
#include "logging.hpp"
#include "helpers.hpp"
#include "arena.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>

#define NODE_TYPE_LIST(E) \
  E(RootNode) E(Module) E(TypeDefinition) E(GenericParameterDefinition) E(GenericOrBuiltinType) E(FunctionDefinition) E(FunctionParameterDefinition) E(StructDefinition) E(StructEntry) E(EnumDefinition) E(EnumEntry) E(UnionDefinition) E(UnionEntry) E(IntLiteral) E(FloatLiteral) E(StringLiteral) E(UndefinedLiteral) E(StructLiteral) E(Variable) E(BinaryOperation) E(UnaryOperation) E(MemberOperation) E(PointerMemberOperation) E(CastOperation) E(Call) E(FunctionDeclaration) E(VariableDeclaration) E(IfElseStatement) E(WhileStatement) E(ExpressionStatement) E(ReturnStatement) E(Block)
enum class NodeType {
  Unknown = 0,
  #define NODE_TYPE_ENUM_DEF(name) name,
  NODE_TYPE_LIST(NODE_TYPE_ENUM_DEF)
};

struct AST;
struct ASTNode {
  TextSpan span;
  bool malformed        = false;
  NodeType type         = NodeType::Unknown;

  ASTNode* parent       = nullptr;
  ASTNode* next_sibling = nullptr;
  ASTNode* first_child  = nullptr;
  ASTNode* last_child   = nullptr;

#ifdef LANG_DEBUG
  virtual std::string debug(const AST& ast) const;
#endif
};

template<typename NodeType>
class ASTChildrenIterator {
  static_assert(std::is_base_of<ASTNode, NodeType>::value);

private:
  struct ASTChildIter {
    ASTChildIter(NodeType* node, size_t count) : cur(node), count(count) { };
    ASTChildIter operator++() {
      cur = cur->next_sibling;
      count++;
      return *this;
    }
    NodeType& operator*() {
      return *cur;
    }
    bool operator!=(ASTChildIter &cmp_childiter) {
      return count != cmp_childiter.count && cur != cmp_childiter.cur;
    }
    NodeType* cur;
    size_t count;
  };

public:
  ASTChildrenIterator(ASTNode* parent, size_t count) : parent(parent), count(count) {}

  ASTChildIter begin() { return ASTChildIter(parent->first_child); }

  ASTChildIter end() { return ASTChildIter(nullptr, count); }

private:
  ASTNode* parent;
  size_t count;
};

// adds a default constructor so we can add in the node type automatically,
// and the debug function.
#ifdef LANG_DEBUG
#define AST_NODE_STRUCT_DEF(name) \
struct name : ASTNode {                                                   \
  name();                                                                 \
  virtual std::string debug(const AST& ast) const;
#else
#define AST_NODE_STRUCT_DEF(name)  \
struct name : ASTNode {                                                   \
  name();
#endif

#define NODE_TYPE_EARLY_DECLARE(name) struct name;
NODE_TYPE_LIST(NODE_TYPE_EARLY_DECLARE)

AST_NODE_STRUCT_DEF(RootNode)  ASTNode* first_module;
  size_t num_modules;
  ASTChildrenIterator<Module> modules();
};

void add_child(ASTNode* parent, ASTNode* child);
void add_parent(ASTNode* child, ASTNode* parent);

void add_child_from_different_source(ASTNode* parent, ASTNode* child);

struct AST {
  RootNode root;

  std::string get_name(name_id id) const;
  name_id get_name_id(std::string name);

  template<typename T>
  T* create_node(TextSpan span) {
    static_assert(std::is_base_of<ASTNode, T>::value);
    T* node = this->arena.allocate<T>();
    ((ASTNode*)node)->span = span;
    return node;
  }

private:
  Arena arena;

  std::unordered_multimap<name_id, std::string> name_id_to_name = {
    { name_id_int, "int" },
    { name_id_float, "float" },
    { name_id_ptr, "ptr" },
    { name_id_buffer, "buffer" },
  };
  name_id id_counter = name_id_num_builtins;
};

void print_ast(const AST& ast);
std::string to_string(const AST& ast, const ASTNode* node);

// a module is the top-level node and contains
// a list of type definition, variable declarations
// and function declarations.
AST_NODE_STRUCT_DEF(Module)
  name_id module_name = -1;
  
  ASTNode* first_top_level_declaration = nullptr;
  size_t num_top_level_declaration;
  ASTChildrenIterator<ASTNode> top_level_declarations();
};

// type expressions. there are two forms:
//  - alias, meaning this is a realisation of 
//    generic type or an alias.
//  - primitive, this defines a new type.
// @todo meta types
union TypeExpression {
  ASTNode*              any;
  GenericOrBuiltinType* alias_or_primitive;
  FunctionDefinition*   function_definition;
  StructDefinition*     struct_definition;
  EnumDefinition*       enum_definition;
  UnionDefinition*      union_definition;
};

union ValueExpression {
  ASTNode*                any;
  IntLiteral*             int_;
  FloatLiteral*           float_;
  StringLiteral*          string;
  StructLiteral*          struct_;
  UndefinedLiteral*       undefined;
  Variable*               variable;
  UnaryOperation*         unary;
  BinaryOperation*        binary;
  MemberOperation*        member;
  PointerMemberOperation* pointer_member;
  CastOperation*          cast;
  Call*                   call;
};

union TypeOrValueExpression {
  ASTNode*        any;
  TypeExpression  type;
  ValueExpression value;
};

// defines a new type with a certain name which should be unique.
// types can be generic, meaning they accept a
// a list of parameters. their parameters can 
// be meta-types (set of types) or types (eg. int).
// when the type is realised these parameters are
// inferred or explicitly provided, in either case
// all parameters must be resolved at compile time.
// @note the list of generic parameters may be
// missing.
AST_NODE_STRUCT_DEF(TypeDefinition)
  name_id identifier = -1;
  
  TypeExpression type_expression;

  GenericParameterDefinition* first_generic_parameter = nullptr;
  size_t num_generic_parameters;
  ASTChildrenIterator<GenericParameterDefinition> generic_parameter_definitions();
};

// defines a parameterisation of a generic type,
// if the type is ommitted it is assumed to be 
// any type (eg. meta-type "Any"). unlike function
// parameters since a parameter can be a meta-type the
// default value can be a type.
AST_NODE_STRUCT_DEF(GenericParameterDefinition)
  name_id parameter_identifier = -1;

  TypeExpression parameter_type;
  TypeOrValueExpression default_value;
};

AST_NODE_STRUCT_DEF(GenericOrBuiltinType)
  name_id reference = -1;

  TypeOrValueExpression first_parameter;
  size_t num_parameters;
  // @todo iterator for union?

  Type* resolved_type = nullptr;
};

// defines a function, if the function is missing
// it's body then the resulting type is a 
// function type, otherwise the type is the function
// itself (ie. not the same as a function with the same prototype).
AST_NODE_STRUCT_DEF(FunctionDefinition)
  FunctionParameterDefinition* first_parameter = nullptr;
  size_t num_parameters;
  ASTChildrenIterator<FunctionParameterDefinition> get_parameters();

  // void is not a type in itself, but rather when return type if missing
  TypeExpression return_type;

  Type* resolved_type = nullptr;
};

AST_NODE_STRUCT_DEF(FunctionParameterDefinition)
  TypeExpression parameter_type;
  ValueExpression default_value;
  name_id identifier = -1;

  llvm::AllocaInst* llvm_alloca_inst = nullptr;
};

// structs
AST_NODE_STRUCT_DEF(StructDefinition)
  StructEntry* first_entry = nullptr;
  size_t num_entries;
  ASTChildrenIterator<StructEntry> get_entries();

  Type* resolved_type = nullptr;
};

AST_NODE_STRUCT_DEF(StructEntry)
  name_id member_name = -1;
  bool is_spread;
  TypeExpression entry_type;
  ValueExpression default_value;
};

// enums
AST_NODE_STRUCT_DEF(EnumDefinition)
  TypeExpression underlying_type;
  EnumEntry* first_entry = nullptr;
  size_t num_entries;
  ASTChildrenIterator<EnumEntry> get_entries();

  Type* resolved_type = nullptr;
};

AST_NODE_STRUCT_DEF(EnumEntry)
  name_id constant_name = -1;
  TypeExpression spread_type;
  ValueExpression value;
};

// unions
AST_NODE_STRUCT_DEF(UnionDefinition)
  UnionEntry* first_entry = nullptr;
  size_t num_entries;
  ASTChildrenIterator<UnionEntry> get_entries();

  Type* resolved_type = nullptr;
};

AST_NODE_STRUCT_DEF(UnionEntry)
  name_id participant_name = -1;
  TypeExpression entry_type;
};

AST_NODE_STRUCT_DEF(IntLiteral)
  int value;
};

AST_NODE_STRUCT_DEF(FloatLiteral) 
  float value;
};

AST_NODE_STRUCT_DEF(StringLiteral) 
  std::string value;
};

AST_NODE_STRUCT_DEF(UndefinedLiteral)
};

AST_NODE_STRUCT_DEF(StructLiteral)  
  // @note any missing entries should be initialized following
  // this struct's default values.
  TypeExpression referencing_type;

  // @todo better way of storing member names
  std::vector<name_id> entry_member_names;
  ValueExpression first_entry;
  size_t num_entries;
};

AST_NODE_STRUCT_DEF(Variable)  
  name_id identifier = -1;
};

#define UNARY_OPERATOR_LIST(E) \
  E(PrefixIncrement)\
  E(PrefixDecrement)\
  E(Plus)\
  E(Minus)\
  E(LogicalNot)\
  E(Dereference)\
  E(AddressOf)\
  E(PostfixIncrement)\
  E(PostfixDecrement)\
  E(Member)\
  E(PointerMember)
enum class UnaryOperator {
  #define UNARY_OPERATOR_ENUM_DEF(name) name,
  UNARY_OPERATOR_LIST(UNARY_OPERATOR_ENUM_DEF)
};


#define BINARY_OPERATOR_LIST(E) \
  E(Plus)\
  E(Minus)\
  E(Multiply)\
  E(Divide)\
  E(Less)\
  E(LessEq)\
  E(Greater)\
  E(GreaterEq)\
  E(Equal)\
  E(NotEqual)\
  E(LogicalAnd)\
  E(LogicalOr)\
  E(Assign)\
  E(PlusAssign)\
  E(MinusAssign)\
  E(MultiplyAssign)\
  E(DivideAssign)\
  E(Subscript)
enum class BinaryOperator {
  #define BINARY_OPERATOR_ENUM_DEF(name) name,
  BINARY_OPERATOR_LIST(BINARY_OPERATOR_ENUM_DEF)
};

std::string to_string(BinaryOperator op);
std::string to_string(UnaryOperator op);

AST_NODE_STRUCT_DEF(BinaryOperation)
  BinaryOperator op;
  ValueExpression lhs;
  ValueExpression rhs;
};

AST_NODE_STRUCT_DEF(UnaryOperation)
  UnaryOperator op;
  ValueExpression operand;
};

AST_NODE_STRUCT_DEF(MemberOperation)
  ValueExpression reference;
  name_id member = -1;
};

AST_NODE_STRUCT_DEF(PointerMemberOperation)
  ValueExpression pointer;
  name_id member = -1;
};

AST_NODE_STRUCT_DEF(CastOperation)
  TypeExpression desired_type;
  ValueExpression value;
};

AST_NODE_STRUCT_DEF(Call)
  TypeOrValueExpression first_generic_parameter;
  size_t num_generic_parameters;
  
  name_id function_name = -1;
  ValueExpression function_pointer;

  ValueExpression first_parameter;
  size_t num_parameters;
};

AST_NODE_STRUCT_DEF(FunctionDeclaration)
  name_id identifier = -1;

  GenericParameterDefinition* first_generic_parameter = nullptr;
  size_t num_generic_parameters;
  ASTChildrenIterator<GenericParameterDefinition> generic_parameter_definitions();

  TypeExpression definition;
  Block* body;
  
  llvm::Function* llvm_function = nullptr;
};

AST_NODE_STRUCT_DEF(VariableDeclaration)
  name_id identifier = -1;
  TypeExpression desired_type;
  ValueExpression value;
};

struct ConditionAndBlock {
  ValueExpression condition;
  Block* block;
};

class ConditionAndBlockIterator {
private:
  struct ASTChildIter {
    ASTChildIter(ASTNode* node) : cur(node) { };
    ASTChildIter operator++() {
      cur = cur->next_sibling->next_sibling;
      return *this;
    }
    ConditionAndBlock operator*() {
      LANG_ASSERT(cur != nullptr);
      LANG_ASSERT(cur->next_sibling != nullptr && cur->next_sibling->type == NodeType::Block);

      return ConditionAndBlock {
        .condition = ValueExpression{.any = cur},
        .block = (Block*)cur->next_sibling,
      };
    }
    bool operator!=(ASTChildIter &cmp_childiter) {
      return cur != cmp_childiter.cur;
    }
    ASTNode* cur;
  };

public:
  ConditionAndBlockIterator(ASTNode* first_condition, ASTNode* else_block) : first_condition(first_condition), else_block(else_block) {}

  ASTChildIter begin() { return ASTChildIter(first_condition); }

  ASTChildIter end() { return ASTChildIter(else_block); }

private:
  ASTNode* first_condition;
  ASTNode* else_block;
};

AST_NODE_STRUCT_DEF(IfElseStatement)
  ValueExpression first_condition;
  ConditionAndBlockIterator condition_and_blocks();
  Block* else_block;

  llvm::BasicBlock* llvm_merged_bb = nullptr;
};

AST_NODE_STRUCT_DEF(WhileStatement)
  ValueExpression condition;
  Block* body;
};

AST_NODE_STRUCT_DEF(ExpressionStatement)
  ValueExpression expression;
};

AST_NODE_STRUCT_DEF(ReturnStatement)
  ValueExpression expression;
};

AST_NODE_STRUCT_DEF(Block)  
  ASTNode* first_statement_or_scope = nullptr;
  size_t num_statements_or_scopes;
  ASTChildrenIterator<ASTNode> statements_or_scope();

  std::string llvm_bb_name = "block";
  llvm::BasicBlock* llvm_bb = nullptr;
  bool dont_create_basic_block = false;
};

#ifdef LANG_DEBUG
template<typename T>
std::string debug_property(const AST& ast, T v) {
  return std::to_string(v);
}
#endif