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

ASTNode& AST::make_node(ASTNodeType type) {
  auto& node = this->nodes.emplace_back(type);
  node.id = this->nodes.size() - 1;
  return node;
}

std::string AST::get_name(name_id id) const {
  auto lu = this->name_id_to_name.find(id);
  LANG_ASSERT(id >= 0 && lu != this->name_id_to_name.end(), "invalid name id");

  return lu->second;
}

name_id AST::get_name_id(std::string name) {
  name_id_to_name.insert({ this->id_counter, name });

  return this->id_counter++;
}

// @note not efficient, prefer fetching in order yourself
ASTNode& get_child(AST& ast, int nodei, int childi) {
  LANG_ASSERT(childi < ast.nodes[nodei].num_children);

  int i = 0;
  for (auto& child : ASTChildrenIterator(ast, nodei)) {
    if (i == childi)
      return child;
  }

  LANG_ASSERT(false);
  return ast.nodes[nodei];
}


// @todo might be better to bake this into the structure
// @note returns original node if there are no children
int last_child(AST& ast, int nodei) {
  int child_id = nodei;
  for (const auto& child : ASTChildrenIterator(ast, nodei)) {
    child_id = child.id;
  }

  return child_id;
}

void add_child(AST& ast, int parent, int child) {
  ast.nodes[parent].size += ast.nodes[child].size;
  ast.nodes[parent].num_children++;
  ast.nodes[child].parent = ast.nodes[parent].id;
  ast.nodes[parent].span.absorb(ast.nodes[child].span);
}

int make_parent(AST& ast, ASTNodeType type, int child) {
  int parent = child;
  ast.nodes.emplace(ast.nodes.begin() + parent, type);
  child = child + 1;

  if (ast.nodes[child].parent >= 0)
    ast.nodes[ast.nodes[child].parent].size++;
  ast.nodes[parent].id = parent;
  ast.nodes[parent].num_children++;
  ast.nodes[parent].size = ast.nodes[child].size + 1;
  ast.nodes[parent].span = ast.nodes[child].span;
  ast.nodes[parent].parent = ast.nodes[child].parent;
  ast.nodes[child].id = child;
  ast.nodes[child].parent = parent;
  
  // shift ids of child's tree
  for (int nodei = child + 1; nodei < child + ast.nodes[child].size; ++nodei) {
    ast.nodes[nodei].id++;
    ast.nodes[nodei].parent++;
  }

  return parent;
}

std::string to_string(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::Plus             : return "Plus";
    case BinaryOperator::Minus            : return "Minus";
    case BinaryOperator::Multiply         : return "Multiply";
    case BinaryOperator::Divide           : return "Divide";
    case BinaryOperator::Less             : return "Less";
    case BinaryOperator::LessEq           : return "LessEq";
    case BinaryOperator::Greater          : return "Greater";
    case BinaryOperator::GreaterEq        : return "GreaterEq";
    case BinaryOperator::NotEqual         : return "NotEqual";
    case BinaryOperator::Equal            : return "Equal";
    case BinaryOperator::LogicalOr        : return "LogicalOr";
    case BinaryOperator::LogicalAnd       : return "LogicalAnd";
    case BinaryOperator::Assign           : return "Assign";
    case BinaryOperator::PlusAssign       : return "PlusAssign";
    case BinaryOperator::MinusAssign      : return "MinusAssign";
    case BinaryOperator::MultiplyAssign   : return "MultiplyAssign";
    case BinaryOperator::DivideAssign     : return "DivideAssign";
    case BinaryOperator::Subscript        : return "Subscript";
  }
  return "Invalid binary operator";
}

std::string to_string(UnaryOperator op) {
  switch (op) {
    case UnaryOperator::PrefixIncrement    : return "PrefixIncrement";
    case UnaryOperator::PrefixDecrement    : return "PrefixDecrement";
    case UnaryOperator::Plus               : return "Plus";
    case UnaryOperator::Minus              : return "Minus";
    case UnaryOperator::LogicalNot         : return "LogicalNot";
    case UnaryOperator::Dereference        : return "Dereference";
    case UnaryOperator::AddressOf          : return "AddressOf";
    case UnaryOperator::PostfixIncrement   : return "PostfixIncrement";
    case UnaryOperator::PostfixDecrement   : return "PostfixDecrement";
    case UnaryOperator::MemberAccess       : return "MemberAccess";
    case UnaryOperator::PointerMemberAccess: return "PointerMemberAccess";
  }
  return "Invalid unary operator";
}

static std::string name_or_unknown(const AST& ast, const ASTNode& node) {
  if (node.name >= 0)
    return ast.get_name(node.name);
  return "#no-name";
}

std::string to_string(const AST& ast, const ASTNode& node) {
  std::string res = to_string(node.type);

  switch (node.type) {
    case ASTNodeType::TypeDefinition:
    {
      auto* data = node.cast_data<ASTTypeDefinitionData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->is_generic ? " is_generic" : "") +
              ")";
      break;
    }
    case ASTNodeType::BinaryOperator:
      res += " (" + to_string(node.cast_data<ASTBinaryOperatorData>()->op) + ")";
      break;
    case ASTNodeType::UnaryOperator:
      res += " (" + to_string(node.cast_data<ASTUnaryOperatorData>()->op) + ")";
      break;
    case ASTNodeType::IntLiteral:
      res +=" (" + std::to_string(node.cast_data<ASTIntLiteralData>()->value) + ")";
      break;
    case ASTNodeType::FloatLiteral:
      res +=" (" + std::to_string(node.cast_data<ASTFloatLiteralData>()->value) + ")";
      break;
    case ASTNodeType::StringLiteral:
      res +=" (" + node.cast_data<ASTStringLiteralData>()->value + ")";
      break;
    case ASTNodeType::Variable:
      res +=" (" + name_or_unknown(ast, node) + ")";
      break;
    case ASTNodeType::Call:
    {
      auto* data = node.cast_data<ASTCallData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->is_function_pointer ? " is_function_pointer" : "") + 
                 + (data->is_generic ? " is_generic" : "") + 
              ")";
      break;
    }
    case ASTNodeType::If:
      if (node.cast_data<ASTIfData>()->has_else)
        res += " (has_else)";
      break;
    case ASTNodeType::Block:
      res +=" (" + node.cast_data<ASTBlockData>()->llvm_bb_name + ")";
      break;
    case ASTNodeType::VariableDeclaration:
      res +=" (" + name_or_unknown(ast, node) + ")";
      break;
    case ASTNodeType::GenericParameterDefinition:
    {
      auto* data = node.cast_data<ASTGenericParameterDefinitionData>();
      res += std::string(" (") + name_or_unknown(ast, node)
                + (data->has_default_value ? " has_default_value" : "") + 
                + (data->has_type ? " has_type" : "") + 
              ")";
      break;
    }
    case ASTNodeType::TypeAlias:
    {
      auto* data = node.cast_data<ASTTypeAliasData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->has_arguments ? " has_arguments" : "") + 
              ")";
      break;
    }
    case ASTNodeType::FunctionDefinition:
      if (node.cast_data<ASTFunctionDefinitionData>()->is_void)
        res += " (is_void)";
      break;
    case ASTNodeType::ParameterDefinition:
    {
      auto* data = node.cast_data<ASTParameterDefinitionData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->has_default_value ? " has_default_value" : "") + 
              ")";
      break;
    }
    case ASTNodeType::StructEntry:
    {
      auto* data = node.cast_data<ASTStructEntryData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->has_default_value ? " has_default_value" : "") + 
                 + (data->has_type ? " has_type" : "") + 
                 + (data->is_spread ? " is_spread" : "") + 
              ")";
      break;
    }
    case ASTNodeType::StructLiteralEntry:
      res +=" (" + name_or_unknown(ast, node) + ")";
      break;
    case ASTNodeType::EnumEntry:
    {
      auto* data = node.cast_data<ASTEnumEntryData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->has_value ? " has_value" : "") + 
                 + (data->is_spread ? " is_spread" : "") + 
              ")";
      break;
    }
    case ASTNodeType::UnionEntry:
    {
      auto* data = node.cast_data<ASTUnionEntryData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->is_spread ? " is_spread" : "") + 
              ")";
      break;
    }
    case ASTNodeType::FunctionDeclaration:
    {
      auto* data = node.cast_data<ASTFunctionDeclarationData>();
      res +=" (" + name_or_unknown(ast, node)
                 + (data->has_type ? " has_type" : "") + 
                 + (data->is_generic ? " is_generic" : "") + 
              ")";
      break;
    }
    case ASTNodeType::GenericParameterDefinitionList:
    case ASTNodeType::GenericParameterList:
    case ASTNodeType::ParameterDefinitionList:
    case ASTNodeType::StructDefinition:
    case ASTNodeType::EnumDefinition:
    case ASTNodeType::UnionDefinition:
    case ASTNodeType::StructLiteral:
    case ASTNodeType::UndefinedLiteral:
    case ASTNodeType::StructLiteralEntryList:
    case ASTNodeType::CastOperator:
    case ASTNodeType::ExpressionStatement:
    case ASTNodeType::Return:
    case ASTNodeType::While:
    case ASTNodeType::Module:
    case ASTNodeType::Unknown:
      break;
  }

  if (!node.span.is_null())
    res += " " + to_string(node.span);

  res +=
    ", id = " + std::to_string(node.id) + 
    ", size = " + std::to_string(node.size) + 
    ", children = " + std::to_string(node.num_children);
  if (node.parent >= 0)
    res += ", parent = " + std::to_string(node.parent);
  if (node.malformed)
    res += ", malformed";

  return res;
}

static void print_ast_recursive(const AST& ast, const ASTNode& node, std::string prepend) {
  std::cout << prepend << "-" << to_string(ast, node) << "\n";

  for (int i = 0, child_id = node.id + 1; i < node.num_children && child_id < ast.nodes.size(); ++i) {
    print_ast_recursive(ast, ast.nodes[child_id], prepend + (i == node.num_children - 1 ? "  " : " |"));
    child_id += ast.nodes[child_id].size;
  }
}

void print_ast(const AST& ast) {
  if (ast.nodes.empty()) {
    std::cout << "AST is empty.\n";
    return;
  }

  print_ast_recursive(ast, ast.nodes[0], "");
}

void construct_data(ASTNode& node) {
  switch(node.type) {
    case ASTNodeType::GenericParameterDefinition:
      node.data = unique_void(new ASTGenericParameterDefinitionData{});
      return;
    case ASTNodeType::TypeDefinition:
      node.data = unique_void(new ASTTypeDefinitionData{});
      return;
    case ASTNodeType::TypeAlias:
      node.data = unique_void(new ASTTypeAliasData{});
      return;
    case ASTNodeType::FunctionDefinition:
      node.data = unique_void(new ASTFunctionDefinitionData{});
      return;
    case ASTNodeType::ParameterDefinition:
      node.data = unique_void(new ASTParameterDefinitionData{});
      return;
    case ASTNodeType::StructDefinition:
      node.data = unique_void(new ASTStructDefinitionData{});
      return;
    case ASTNodeType::StructEntry:
      node.data = unique_void(new ASTStructEntryData{});
      return;
    case ASTNodeType::EnumDefinition:
      node.data = unique_void(new ASTEnumDefinitionData{});
      return;
    case ASTNodeType::EnumEntry:
      node.data = unique_void(new ASTEnumEntryData{});
      return;
    case ASTNodeType::UnionDefinition:
      node.data = unique_void(new ASTUnionDefinitionData{});
      return;
    case ASTNodeType::UnionEntry:
      node.data = unique_void(new ASTUnionEntryData{});
      return;
    case ASTNodeType::IntLiteral:
      node.data = unique_void(new ASTIntLiteralData{});
      return;
    case ASTNodeType::FloatLiteral:
      node.data = unique_void(new ASTFloatLiteralData{});
      return;
    case ASTNodeType::StringLiteral:
      node.data = unique_void(new ASTStringLiteralData{});
      return;
    case ASTNodeType::StructLiteral:
      node.data = unique_void(new ASTStructLiteralData{});
      return;
    case ASTNodeType::BinaryOperator:
      node.data = unique_void(new ASTBinaryOperatorData{});
      return;
    case ASTNodeType::UnaryOperator:
      node.data = unique_void(new ASTUnaryOperatorData{});
      return;
    case ASTNodeType::CastOperator:
      node.data = unique_void(new ASTCastOperatorData{});
      return;
    case ASTNodeType::Call:
      node.data = unique_void(new ASTCallData{});
      return;
    case ASTNodeType::VariableDeclaration:
      node.data = unique_void(new ASTVariableDeclarationData{});
      return;
    case ASTNodeType::FunctionDeclaration:
      node.data = unique_void(new ASTFunctionDeclarationData{});
      return;
    case ASTNodeType::If:
      node.data = unique_void(new ASTIfData{});
      return;
    case ASTNodeType::Block:
      node.data = unique_void(new ASTBlockData{});
      return;
    default:
      return;
  }
}

ASTNode::ASTNode(ASTNodeType _type) : data(unique_void((void*)nullptr)), type(_type) {
  construct_data(*this);
}

std::string to_string(const ASTNodeType& type) {
  switch (type) {
    case ASTNodeType::Unknown                       : return "Unknown";
    case ASTNodeType::Module                        : return "Module";
    case ASTNodeType::TypeDefinition                : return "TypeDefinition";
    case ASTNodeType::GenericParameterDefinitionList: return "GenericParameterDefinitionList";
    case ASTNodeType::GenericParameterDefinition    : return "GenericParameterDefinition";
    case ASTNodeType::TypeAlias                     : return "TypeAlias";
    case ASTNodeType::GenericParameterList          : return "GenericParameterList";
    case ASTNodeType::FunctionDefinition            : return "FunctionDefinition";
    case ASTNodeType::ParameterDefinitionList       : return "ParameterDefinitionList";
    case ASTNodeType::ParameterDefinition           : return "ParameterDefinition";
    case ASTNodeType::StructDefinition              : return "StructDefinition";
    case ASTNodeType::StructEntry                   : return "StructEntry";
    case ASTNodeType::EnumDefinition                : return "EnumDefinition";
    case ASTNodeType::EnumEntry                     : return "EnumEntry";
    case ASTNodeType::UnionDefinition               : return "UnionDefinition";
    case ASTNodeType::UnionEntry                    : return "UnionEntry";
    case ASTNodeType::IntLiteral                    : return "IntLiteral";
    case ASTNodeType::FloatLiteral                  : return "FloatLiteral";
    case ASTNodeType::StringLiteral                 : return "StringLiteral";
    case ASTNodeType::UndefinedLiteral              : return "UndefinedLiteral";
    case ASTNodeType::StructLiteral                 : return "StructLiteral";
    case ASTNodeType::StructLiteralEntryList        : return "StructLiteralEntryList";
    case ASTNodeType::StructLiteralEntry            : return "StructLiteralEntry";
    case ASTNodeType::Variable                      : return "Variable";
    case ASTNodeType::BinaryOperator                : return "BinaryOperator";
    case ASTNodeType::UnaryOperator                 : return "UnaryOperator";
    case ASTNodeType::CastOperator                  : return "CastOperator";
    case ASTNodeType::Call                          : return "Call";
    case ASTNodeType::FunctionDeclaration           : return "FunctionDeclaration";
    case ASTNodeType::VariableDeclaration           : return "VariableDeclaration";
    case ASTNodeType::Return                        : return "Return";
    case ASTNodeType::ExpressionStatement           : return "ExpressionStatement";
    case ASTNodeType::If                            : return "If";
    case ASTNodeType::While                         : return "While";
    case ASTNodeType::Block                         : return "Block";
  }
}
