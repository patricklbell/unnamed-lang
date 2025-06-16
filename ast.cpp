#include "ast.hpp"
#include "files.hpp"
#include "logging.hpp"
#include "lexer.hpp"
#include <iostream>
#include <ostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

std::string AST::get_name(name_id id) const {
  auto lu = this->name_id_to_name.find(id);
  LANG_ASSERT(id >= 0 && lu != this->name_id_to_name.end(), "invalid name id");

  return lu->second;
}

name_id AST::get_name_id(std::string name) {
  name_id_to_name.insert({ this->id_counter, name });

  return this->id_counter++;
}

void add_child_from_different_source(ASTNode* parent, ASTNode* child) {
  if (parent->last_child == nullptr) {
    parent->first_child = child;
    parent->last_child = child;
  } else {
    parent->last_child->next_sibling = child;
    parent->last_child = child;
  }

  child->parent = parent;
  child->next_sibling = nullptr;
}

// @note assumes child has not been added to the tree already
void add_child(ASTNode* parent, ASTNode* child) {
  add_child_from_different_source(parent, child);
  parent->span.absorb(child->span);
}

// @note assumes parent has not been added to the tree already
void add_parent(ASTNode* child, ASTNode* parent) {
  parent->parent = child->parent;
  parent->first_child = child;
  parent->last_child = child;
  parent->next_sibling = child->next_sibling;
  // correct previous sibling
  if (parent->parent != nullptr) {
    for (auto sibling = parent->parent->first_child; sibling != nullptr; sibling = sibling->next_sibling) {
      if (sibling->next_sibling == child) {
        sibling->next_sibling = parent;
      }
    }
  }

  child->parent = parent;
  child->next_sibling = nullptr;
}

std::string to_string(BinaryOperator op) {
  switch (op) {
    #define BINARY_OPERATOR_SWITCH_DEF(name) case BinaryOperator::name: return #name;
    BINARY_OPERATOR_LIST(BINARY_OPERATOR_SWITCH_DEF)
  }
  return "Invalid binary operator";
}

std::string to_string(UnaryOperator op) {
  switch (op) {
    #define UNARY_OPERATOR_SWITCH_DEF(name) case UnaryOperator::name: return #name;
    UNARY_OPERATOR_LIST(UNARY_OPERATOR_SWITCH_DEF)
  }
  return "Invalid unary operator";
}

std::string to_string(const NodeType& type) {
  switch (type) {
    case NodeType::Unknown: return "Unknown";
    #define NODE_TYPE_SWITCH_DEF(name) case NodeType::name: return #name;
    NODE_TYPE_LIST(NODE_TYPE_SWITCH_DEF)
  }
}

static std::string safe_get_name(const AST& ast, name_id id) {
  if (id >= 0)
    return ast.get_name(id);
  return "#no-name";
}

std::string to_string(const AST& ast, const ASTNode* node) {
  if (node == nullptr)
  return "nullptr";
  
  #ifdef LANG_DEBUG
  // in debug, a virtual function call is simpler here
  return node->debug(ast);
  #else
  return to_string(node->type);
  #endif
}

static void print_ast_recursive(const AST& ast, const ASTNode* node, std::string prepend) {
  std::cout << prepend << "-" << to_string(ast, node) << "\n";

  for (auto child = node->first_child; child != nullptr; child = child->next_sibling) {
    print_ast_recursive(ast, child, prepend + (child == node->last_child ? "  " : " |"));
  }
}

void print_ast(const AST& ast) {
  print_ast_recursive(ast, &ast.root, "");
}

// initialize correct node type in default constructors
#define NODE_TYPE_DEFAULT_INITIALIZER_IMPL(name) \
  name::name() { this->type = NodeType::name; }
NODE_TYPE_LIST(NODE_TYPE_DEFAULT_INITIALIZER_IMPL)

// custom debug printing
#ifdef LANG_DEBUG
std::string debug_property(const AST& ast, size_t count) {
  return std::to_string(count);
}
std::string debug_property(const AST& ast, ASTNode* node) {
  return node == nullptr ? "nullptr" : to_string(node->type);
}
std::string debug_property(const AST& ast, ValueExpression node) {
  return node.any == nullptr ? "nullptr" : to_string(node.any->type);
}
std::string debug_property(const AST& ast, TypeExpression node) {
  return node.any == nullptr ? "nullptr" : to_string(node.any->type);
}
std::string debug_property(const AST& ast, name_id id) {
  return id <= 0 ? "#unset" : ast.get_name(id);
}
std::string debug_property(const AST& ast, bool property) {
  return property ? "true" : "false";
}
std::string debug_property(const AST& ast, BinaryOperator op) {
  return to_string(op);
}
std::string debug_property(const AST& ast, UnaryOperator op) {
  return to_string(op);
}
std::string debug_property(const AST& ast, std::string str) {
  auto line_break = str.find('\n');
  if (line_break != std::string::npos)
    return "\"" + str.substr(0, line_break) + "...\"";  
  return "\"" + str + "\"";
}

#define PROPERTY(member) (#member + std::string("=") + debug_property(ast, this->member))

std::string print_list(std::initializer_list<std::string> list)
{
  if (list.begin() == list.end())
    return "";

  std::string result = *list.begin();
  for(auto itr = list.begin() + 1; itr != list.end(); ++itr) {
    result += ", " + *itr;
  };
  
  return ": " + result;
}

std::string ASTNode::debug(const AST& ast) const {
  return to_string(type);
}
std::string RootNode::debug(const AST& ast) const {
  return to_string(type);
}
std::string Module::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(module_name)});
}
std::string TypeDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_generic_parameters)});
}
std::string GenericParameterDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(parameter_identifier)});
}
std::string GenericOrBuiltinType::debug(const AST& ast) const {
  return to_string(type);
}
std::string FunctionDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_parameters), PROPERTY(return_type)});
}
std::string FunctionParameterDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(default_value), PROPERTY(parameter_type)});
}
std::string StructDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_entries)});
}
std::string StructEntry::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(member_name), PROPERTY(entry_type), PROPERTY(default_value), PROPERTY(is_spread)});
}
std::string EnumDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_entries)});
}
std::string EnumEntry::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(constant_name), PROPERTY(spread_type), PROPERTY(value)});
}
std::string UnionDefinition::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_entries)});
}
std::string UnionEntry::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(participant_name), PROPERTY(entry_type)});
}
std::string IntLiteral::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(value)});
}
std::string FloatLiteral::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(value)});
}
std::string StringLiteral::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(value)});
}
std::string UndefinedLiteral::debug(const AST& ast) const {
  return to_string(type);
}
std::string StructLiteral::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_entries)});
}
std::string Variable::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(identifier)});
}
std::string BinaryOperation::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(op)});
}
std::string UnaryOperation::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(op)});
}
std::string MemberOperation::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(member)});
}
std::string PointerMemberOperation::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(member)});
}
std::string CastOperation::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(desired_type)});
}
std::string Call::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(num_parameters), PROPERTY(num_generic_parameters)});
}
std::string FunctionDeclaration::debug(const AST& ast) const {
  return to_string(type) + print_list({PROPERTY(identifier), PROPERTY(definition), PROPERTY(num_generic_parameters)});
}
std::string VariableDeclaration::debug(const AST& ast) const {
  return to_string(type) + PROPERTY(identifier);
}
std::string IfElseStatement::debug(const AST& ast) const {
  return to_string(type);
}
std::string WhileStatement::debug(const AST& ast) const {
  return to_string(type);
}
std::string ExpressionStatement::debug(const AST& ast) const {
  return to_string(type);
}
std::string ReturnStatement::debug(const AST& ast) const {
  return to_string(type);
}
std::string Block::debug(const AST& ast) const {
  return to_string(type);
}
#endif