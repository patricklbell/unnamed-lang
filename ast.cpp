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

// @todo avoid allocating unkown data twice using class constructor
ASTNode::ASTNode(ASTNodeType _type) : data(unique_void(new ASTUnkownData{})) {
  type = _type;
  switch(type) {
    case ASTNodeType::Unknown:
       malformed = true;
       data = unique_void(new ASTUnkownData{});
       return;  
    case ASTNodeType::Return:
       data = unique_void(new ASTReturnData{});
       return;
    case ASTNodeType::VariableDefinition:
       data = unique_void(new ASTVariableDefinitionData{});
       return;
    case ASTNodeType::ExpressionStatement:
       data = unique_void(new ASTExpressionStatementData{});
       return;
    case ASTNodeType::IntLiteral:
       data = unique_void(new ASTIntLiteralData{});
       return;
    case ASTNodeType::FloatLiteral:
       data = unique_void(new ASTFloatLiteralData{});
       return;
    case ASTNodeType::Variable:
       data = unique_void(new ASTVariableData{});
       return;
    case ASTNodeType::BinaryOperator:
       data = unique_void(new ASTBinaryOperatorData{});
       return;
    case ASTNodeType::UnaryOperator:
       data = unique_void(new ASTUnaryOperatorData{});
       return;
    case ASTNodeType::Call:
       data = unique_void(new ASTCallData{});
       return;
    case ASTNodeType::Assignment:
       data = unique_void(new ASTAssignmentData{});
       return;
    case ASTNodeType::If:
       data = unique_void(new ASTIfData{});
       return;
    case ASTNodeType::While:
       data = unique_void(new ASTWhileData{});
       return;
    case ASTNodeType::Prototype:
       data = unique_void(new ASTPrototypeData{});
       return;
    case ASTNodeType::FunctionDefinition:
       data = unique_void(new ASTFunctionDefinitionData{});
       return;
    case ASTNodeType::Block:
       data = unique_void(new ASTBlockData{});
       return;
    case ASTNodeType::Module:
       data = unique_void(new ASTModuleData{});
       return;
  }
  LANG_ASSERT(false, "Unhandled node type");
}

std::string to_string(const ASTNodeType& type) {
  switch (type) {
    case ASTNodeType::Unknown:                return "Unknown";
    case ASTNodeType::Return:                 return "Return";
    case ASTNodeType::Call:                   return "Call";
    case ASTNodeType::Assignment:             return "Assignment";
    case ASTNodeType::IntLiteral:             return "IntLiteral";
    case ASTNodeType::FloatLiteral:           return "FloatLiteral";
    case ASTNodeType::Variable:               return "Variable";
    case ASTNodeType::BinaryOperator:         return "BinaryOperator";
    case ASTNodeType::UnaryOperator:          return "UnaryOperator";
    case ASTNodeType::If:                     return "If";
    case ASTNodeType::While:                  return "While";
    case ASTNodeType::Prototype:              return "Prototype";
    case ASTNodeType::Block:                  return "Block";
    case ASTNodeType::FunctionDefinition:     return "FunctionDefinition";
    case ASTNodeType::ExpressionStatement:    return "ExpressionStatement";
    case ASTNodeType::VariableDefinition:     return "VariableDefinition";
    case ASTNodeType::Module:                 return "Module";
  }
}

ASTNode& AST::make_node(ASTNodeType type) {
  auto& node = this->nodes.emplace_back(type);
  node.id = this->nodes.size() - 1;
  return node;
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
    case BinaryOperator::Plus:        return "Plus";
    case BinaryOperator::Minus:       return "Minus";
    case BinaryOperator::Multiply:    return "Multiply";
    case BinaryOperator::Divide:      return "Divide";
    case BinaryOperator::Less:        return "Less";
    case BinaryOperator::LessEq:      return "LessEq";
    case BinaryOperator::Greater:     return "Greater";
    case BinaryOperator::GreaterEq:   return "GreaterEq";
    case BinaryOperator::Equal:       return "Equal";
    case BinaryOperator::LogicalOr:   return "LogicalOr";
    case BinaryOperator::LogicalAnd:  return "LogicalAnd";
  }
  return "Invalid binary operator";
}

std::string to_string(UnaryOperator op) {
  switch (op) {
    case UnaryOperator::Plus:         return "Plus";
    case UnaryOperator::Minus:        return "Minus";
    case UnaryOperator::LogicalNot:   return "LogicalNot";
  }
  return "Invalid unary operator";
}

std::string to_string(const ASTNode& node) {
  std::string res = to_string(node.type);

  switch (node.type) {
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
    case ASTNodeType::Variable:
      res +=" (" + node.cast_data<ASTVariableData>()->name + ")";
      break;
    case ASTNodeType::Call:
      res +=" (" + node.cast_data<ASTCallData>()->name + ")";
      break;
    case ASTNodeType::Assignment:
      res +=" (" + node.cast_data<ASTAssignmentData>()->name + ")";
      break;
    case ASTNodeType::If:
      res +=" (" + std::to_string(node.cast_data<ASTIfData>()->has_else) + ")";
      break;
    case ASTNodeType::Block:
      res +=" (" + node.cast_data<ASTBlockData>()->llvm_bb_name + ")";
      break;
    case ASTNodeType::Prototype: {
      auto data = node.cast_data<ASTPrototypeData>();
      res +=" (" + data->name + ", args = {";
      for (auto i = data->args.begin(); i != data->args.end(); ++i) 
        res += (i != data->args.begin() ? ", " : "") + i->name;
      res += "})";
      break;
    }
    case ASTNodeType::VariableDefinition:
      res +=" (" + node.cast_data<ASTVariableDefinitionData>()->name + ")";
      break;
    default:
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
  std::cout << prepend << "-" << to_string(node) << "\n";

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
