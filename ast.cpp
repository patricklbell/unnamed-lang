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

// Helpers
static void add_child(AST& ast, int parent, int child) {
  ast.nodes[parent].size += ast.nodes[child].size;
  ast.nodes[parent].num_children++;
  ast.nodes[child].parent = ast.nodes[parent].id;
  ast.nodes[parent].span.absorb(ast.nodes[child].span);
}

static int make_parent(AST& ast, ASTNodeType type, int child) {
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

static int make_node(AST& ast, ASTNodeType type, Lexer& lexer) {
  auto& node = ast.make_node(type);
  node.span = lexer.peek_span();
  return node.id;
}

static bool expect_token(const TokenType& expected, Lexer& lexer) {
  if (lexer.peek_type() != expected) {
    return false;
  }

  lexer.consume_token();
  return true;
}

static bool expect_token(ASTNode& node, const TokenType& expected, Lexer& lexer) {
  if (lexer.peek_type() != expected) {
    node.malformed = true;
    return false;
  }

  node.span.absorb(lexer.peek_span());
  lexer.consume_token();
  return true;
}

static bool not_end(Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context = "") {
  if (lexer.peek_type() == TokenType::End) {
    logger.log(Errors::Syntax, context != "" ? ("Unexpectedly reached end of file while parsing " + context + ".") : "Unexpectedly reached end of file.", ast.nodes[nodei].span);
    ast.nodes[nodei].malformed = true;
    return false;
  }

  return true;
}

// Parsing
static void parse_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, int parent_precedence = -1, bool in_parenthesis = false, bool in_comma_list = false);
static int parse_block(AST& ast, Lexer& lexer, Logger& logger);

static bool match_end_expression(const Lexer& lexer, bool in_parenthesis = false, bool in_comma_list = false) {
  return (
    (in_comma_list && lexer.peek_type() == TokenType::Comma)             ||
    (in_parenthesis && lexer.peek_type() == TokenType::RightParenthesis) ||
    lexer.peek_type() == TokenType::Semicolon                            ||
    lexer.peek_type() == TokenType::LeftBrace                            ||
    lexer.peek_type() == TokenType::RightBrace                           ||
    lexer.peek_type() == TokenType::End
  );
}

static int parse_variable(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Variable, lexer);
  ASTVariableData* data = ast.nodes[nodei].cast_data<ASTVariableData>();

  {
    auto name = lexer.peek_token();
    if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
      logger.log(Errors::Syntax, "Variable expected a name.", name.span);
      return nodei;
    }

    data->name = name.value;
  }

  return nodei;
}

static bool match_variable(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Name;
}

static int parse_literal(AST& ast, Lexer& lexer, Logger& logger) {
  auto t = lexer.peek_token();
  lexer.consume_token();

  if (t.type == TokenType::Integer) {
    int nodei = make_node(ast, ASTNodeType::IntLiteral, lexer);
    auto& node = ast.nodes[nodei];

    node.cast_data<ASTIntLiteralData>()->value = std::stoi(t.value);
    node.span.absorb(t.span);

    return nodei;
  }
  else if (t.type == TokenType::Float) {
    int nodei = make_node(ast, ASTNodeType::FloatLiteral, lexer);
    auto& node = ast.nodes[nodei];

    node.cast_data<ASTFloatLiteralData>()->value = std::stof(t.value);
    node.span.absorb(t.span);
    
    return nodei;
  }
  
  LANG_ASSERT("Missing parsing for matched literal");
  return -1;
}

static bool match_literal(const Lexer& lexer) {
  return (
    lexer.peek_type() == TokenType::Float ||
    lexer.peek_type() == TokenType::Integer
  );
}

static int parse_call(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Call, lexer);
  ASTCallData* data = ast.nodes[nodei].cast_data<ASTCallData>();

  {
    auto name = lexer.peek_token();
    if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
      logger.log(Errors::Syntax, "Call expected function name.", name.span);
      return nodei;
    }
    data->name = name.value;
  }

  if (!expect_token(ast.nodes[nodei], TokenType::LeftParenthesis, lexer)) {
    logger.log(Errors::Syntax, "Call expected argument list '('.", lexer.peek_span());
    return nodei;
  }

  while (not_end(lexer, logger, ast, nodei, "call")) {
    if (lexer.peek_type() == TokenType::RightParenthesis) {
      ast.nodes[nodei].span.absorb(lexer.peek_span());
      lexer.consume_token();
      break;
    }

    parse_expression_add_child(ast, lexer, logger, nodei, -1, true, true);

    auto nextt = lexer.peek_type();
    if (nextt == TokenType::Comma) {
      lexer.consume_token();
      continue;
    }
    if (nextt == TokenType::RightParenthesis) {
      continue;
    }

    ast.nodes[nodei].malformed = true;
    logger.log(Errors::Syntax, "Expected ',' between arguments.", lexer.peek_span());
    break;
  }

  return nodei;
}

static bool match_call(const Lexer& lexer) {
  return (
    lexer.peek_type(0) == TokenType::Name &&
    lexer.peek_type(1) == TokenType::LeftParenthesis
  );
}

static int parse_assignment(AST& ast, Lexer& lexer, Logger& logger, bool in_parenthesis, bool in_comma_list) {
  int nodei = make_node(ast, ASTNodeType::Assignment, lexer);
  ASTAssignmentData* data = ast.nodes[nodei].cast_data<ASTAssignmentData>();

  {
    auto name = lexer.peek_token();
    if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
      logger.log(Errors::Syntax, "Assignment expected variable name.", name.span);
      return nodei;
    }
    data->name = name.value;
  }

  if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer)) {
    logger.log(Errors::Syntax, "Call expected '='.", lexer.peek_span());
    return nodei;
  }

  parse_expression_add_child(ast, lexer, logger, nodei, -1, in_parenthesis, in_comma_list);

  return nodei;
}

static bool match_assignment(const Lexer& lexer) {
  return (
    lexer.peek_type(0) == TokenType::Name &&
    lexer.peek_type(1) == TokenType::Equals
  );
}

static void parse_parenthesis_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti) {
  TextSpan maybe_left_parenthesis_span = lexer.peek_span();
  bool has_left_parenthesis = expect_token(TokenType::LeftParenthesis, lexer);

  if (!has_left_parenthesis)
    logger.log(Errors::Syntax, "Expected '('.", lexer.peek_span());

  parse_expression_add_child(ast, lexer, logger, parenti, -1, has_left_parenthesis, false);

  if (has_left_parenthesis && !expect_token(TokenType::RightParenthesis, lexer))
    logger.log(Errors::Syntax, "Unmatched parenthesis.", maybe_left_parenthesis_span);
}

static bool match_parenthesis_expression(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::LeftParenthesis;
}

static void parse_expression_atom_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, bool in_parenthesis, bool in_comma_list) {
  if (match_literal(lexer)) {
    add_child(ast, parenti, parse_literal(ast, lexer, logger));
  } else if (match_parenthesis_expression(lexer)) {
    parse_parenthesis_expression_add_child(ast, lexer, logger, parenti);
  } else if (match_call(lexer)) {
    add_child(ast, parenti, parse_call(ast, lexer, logger));
  } else if (match_assignment(lexer)) {
    add_child(ast, parenti, parse_assignment(ast, lexer, logger, in_parenthesis, in_comma_list));
  } else if (match_variable(lexer)) {
    add_child(ast, parenti, parse_variable(ast, lexer, logger));
  } else if (match_end_expression(lexer)) {
    add_child(ast, parenti, make_node(ast, ASTNodeType::Unknown, lexer));
    logger.log(Errors::Syntax, "Unexpected end of expression.", lexer.peek_span());
  } else {
    add_child(ast, parenti, make_node(ast, ASTNodeType::Unknown, lexer));
    logger.log(Errors::Syntax, "Unexpected token in expression.", lexer.peek_span());
  }
}

static std::unordered_map<TokenType, std::pair<int, BinaryOperator>> binary_operator_precedence_table = {
  {TokenType::Asterisk,         {3,   BinaryOperator::Multiply}},
  {TokenType::ForwardSlash,     {3,   BinaryOperator::Divide}},
  {TokenType::Plus,             {4,   BinaryOperator::Plus}},
  {TokenType::Minus,            {4,   BinaryOperator::Minus}},
  {TokenType::Less,             {6,   BinaryOperator::Less}},
  {TokenType::LessEqual,        {6,   BinaryOperator::LessEq}},
  {TokenType::Greater,          {6,   BinaryOperator::Greater}},
  {TokenType::GreaterEqual,     {6,   BinaryOperator::GreaterEq}},
  {TokenType::DoubleEquals,     {6,   BinaryOperator::Equal}},
  {TokenType::DoubleAnd,        {11,  BinaryOperator::LogicalAnd}},
  {TokenType::DoublePipe,       {12,  BinaryOperator::LogicalOr}},
};

// @note assumes LHS has been parsed
static int parse_binary_operation(AST& ast, Lexer& lexer, Logger& logger, int lhsi, int& parent_precedence, bool in_parenthesis, bool in_comma_list) {
  auto precedence_binop = binary_operator_precedence_table.find(lexer.peek_type())->second;

  // if the precedence of the operator is lower then return the parent
  // if it's equal then return (left to right associativity)
  int precedence = precedence_binop.first; 
  if (parent_precedence != -1 && precedence >= parent_precedence) {
    parent_precedence = -1;
    return ast.nodes[lhsi].parent;
  }

  // create a parent for the lhs
  int nodei = make_parent(ast, ASTNodeType::BinaryOperator, lhsi);
  ASTBinaryOperatorData* data = ast.nodes[nodei].cast_data<ASTBinaryOperatorData>();

  data->op = precedence_binop.second;
  lexer.consume_token();

  // parse the rhs greedily
  parse_expression_add_child(ast, lexer, logger, nodei, precedence, in_parenthesis, in_comma_list);

  {
    // correct the size from tree editing after adding a child
    auto& lhs = ast.nodes[nodei + 1];
    auto& rhs = ast.nodes[lhs.id + lhs.size];
    ast.nodes[nodei].size = 1 + lhs.size + rhs.size;
  }

  return nodei;
}

static bool match_binary_operator(Lexer& lexer) {
  return binary_operator_precedence_table.find(lexer.peek_type()) != binary_operator_precedence_table.end();
}

static std::unordered_map<TokenType, UnaryOperator> unary_operator_table = {
  {TokenType::Plus,             UnaryOperator::Plus},
  {TokenType::Minus,            UnaryOperator::Minus},
  {TokenType::Exclamation,      UnaryOperator::LogicalNot},
};

static bool match_unary_operator(Lexer& lexer) {
  return unary_operator_table.find(lexer.peek_type()) != unary_operator_table.end();
}

static int parse_prefix_unary_operation(AST& ast, Lexer& lexer, Logger& logger, int parenti, bool in_parenthesis, bool in_comma_list) {
  int nodei = make_node(ast, ASTNodeType::UnaryOperator, lexer);
  ASTUnaryOperatorData* data = ast.nodes[nodei].cast_data<ASTUnaryOperatorData>();

  data->op = unary_operator_table.find(lexer.peek_type())->second;
  lexer.consume_token();

  // @note precedence is always right to left for unary operators
  if (match_unary_operator(lexer))
    // allow multiple unary operators eg. !!x
    add_child(ast, nodei, parse_prefix_unary_operation(ast, lexer, logger, nodei, in_parenthesis, in_comma_list));
  else
    parse_expression_atom_add_child(ast, lexer, logger, nodei, in_parenthesis, in_comma_list);

  return nodei;
}

static void parse_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, int parent_precedence, bool in_parenthesis, bool in_comma_list) {
  int parent_original_size = ast.nodes[parenti].size;

  // add at least one atom and use this as the lhs for any binary expressions
  int lhsi = ast.nodes[parenti].id + ast.nodes[parenti].size;
  if (match_unary_operator(lexer)) {
    add_child(ast, parenti, parse_prefix_unary_operation(ast, lexer, logger, parenti, in_parenthesis, in_comma_list));
  } else {
    parse_expression_atom_add_child(ast, lexer, logger, parenti, in_parenthesis, in_comma_list);
  }

  // greedily capture operators, replacing the parent as we go
  while (match_binary_operator(lexer)) {
    lhsi = parse_binary_operation(ast, lexer, logger, lhsi, parent_precedence, in_parenthesis, in_comma_list);
  }

  // @note this will require separators between expression
  if (!match_end_expression(lexer, in_parenthesis, in_comma_list)) {
    logger.log(Errors::Syntax, "Expected end of expression.", lexer.peek_span());
  }
  while (!match_end_expression(lexer, in_parenthesis, in_comma_list)) {
    ast.nodes[parenti].span.absorb(lexer.peek_span());
    lexer.consume_token();
  }

  // since we added the child and then modified the tree, the size may be wrong and needs to be corrected
  ast.nodes[parenti].size = parent_original_size + ast.nodes[lhsi].size;
  ast.nodes[parenti].span.absorb(ast.nodes[lhsi].span);
}

static bool match_variable_definition(const Lexer& lexer) {
  return (
    lexer.peek_type(0) == TokenType::Name &&
    lexer.peek_type(1) == TokenType::Colon
  );
}

static int parse_variable_definition(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::VariableDefinition, lexer);
  ASTVariableDefinitionData* data = ast.nodes[nodei].cast_data<ASTVariableDefinitionData>();

  {
    auto name = lexer.peek_token();
    if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
      logger.log(Errors::Syntax, "Variable definition expected variable name.", name.span);
      return nodei;
    }
    data->name = name.value;
  }

  if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer)) {
    logger.log(Errors::Syntax, "Variable definition expected ':=' or ': [type] =.", lexer.peek_span());
    return nodei;
  }

  if (lexer.peek_type() == TokenType::Name) {
    auto type = lexer.peek_token();
    if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
      logger.log(Errors::Syntax, "Expected a type name.", type.span);
      return nodei;
    }
    data->type = type.value;
  }
  
  if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer)) {
    logger.log(Errors::Syntax, "Variable definition expected ':=' or ': [type] =.", lexer.peek_span());
    return nodei;
  }

  parse_expression_add_child(ast, lexer, logger, nodei);

  return nodei;
}

static bool match_return(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Return;
}

static int parse_return(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Return, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::Return, lexer)) {
    logger.log(Errors::Syntax, "Return expected.", lexer.peek_span());
    return nodei;
  }

  parse_expression_add_child(ast, lexer, logger, nodei);

  return nodei;
}

static bool match_if(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::If;
}

static int parse_if(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::If, lexer);
  ASTIfData* data = ast.nodes[nodei].cast_data<ASTIfData>();

  if (!expect_token(ast.nodes[nodei], TokenType::If, lexer)) {
    logger.log(Errors::Syntax, "If expected.", lexer.peek_span());
    return nodei;
  }

  parse_expression_add_child(ast, lexer, logger, nodei);

  int if_blocki = parse_block(ast, lexer, logger);
  ast.nodes[if_blocki].cast_data<ASTBlockData>()->llvm_bb_name = "if";
  add_child(ast, nodei, if_blocki);

  while (not_end(lexer, logger, ast, nodei, "if")) {
    if (
      lexer.peek_type(0) == TokenType::Else &&
      lexer.peek_type(1) == TokenType::If
    ) {
      lexer.consume_token();
      lexer.consume_token();

      parse_expression_add_child(ast, lexer, logger, nodei);

      int blocki = parse_block(ast, lexer, logger);
      ast.nodes[blocki].cast_data<ASTBlockData>()->llvm_bb_name = "elseif";
      add_child(ast, nodei, blocki);

      continue;
    }

    break;
  }

  data->has_else = lexer.peek_type() == TokenType::Else;
  if (data->has_else) {
    lexer.consume_token();

    int blocki = parse_block(ast, lexer, logger);
    ast.nodes[blocki].cast_data<ASTBlockData>()->llvm_bb_name = "else";
    add_child(ast, nodei, blocki);
  }

  return nodei;
}

static bool match_while(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::While;
}

static int parse_while(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::While, lexer);
  ASTWhileData* data = ast.nodes[nodei].cast_data<ASTWhileData>();

  if (!expect_token(ast.nodes[nodei], TokenType::While, lexer)) {
    logger.log(Errors::Syntax, "While expected.", lexer.peek_span());
    return nodei;
  }

  parse_expression_add_child(ast, lexer, logger, nodei);

  add_child(ast, nodei, parse_block(ast, lexer, logger));

  return nodei;
}

static int parse_expression_statement(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::ExpressionStatement, lexer);

  // make sure expression consumes some tokens so that parse doesn't get stuck on blocks
  int pos_before = lexer.peek_span().start.pos;

  parse_expression_add_child(ast, lexer, logger, nodei);

  if (pos_before == lexer.peek_span().start.pos)
    lexer.consume_token();

  return nodei;
}

static int parse_statement(AST& ast, Lexer& lexer, Logger& logger) {
  if (match_if(lexer)) {
    return parse_if(ast, lexer, logger);
  } else if (match_while(lexer)) {
    return parse_while(ast, lexer, logger);
  }

  int nodei;
  if (match_return(lexer)) {
    nodei = parse_return(ast, lexer, logger);
  } else if (match_variable_definition(lexer)) {
    nodei = parse_variable_definition(ast, lexer, logger);
  } else {
    nodei = parse_expression_statement(ast, lexer, logger);
  }

  if (!expect_token(ast.nodes[nodei], TokenType::Semicolon, lexer))
    logger.log(Errors::Syntax, "Missing semi-colon at the end of statement.", ast.nodes[nodei].span);

  return nodei;
}

static int parse_prototype(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Prototype, lexer);
  ASTPrototypeData* data = ast.nodes[nodei].cast_data<ASTPrototypeData>();

  {
    auto name = lexer.peek_token();
    if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
      logger.log(Errors::Syntax, "Expected function name.", name.span);
      return nodei;
    }
    data->name = name.value;
  }

  if (!expect_token(ast.nodes[nodei], TokenType::LeftParenthesis, lexer)) {
    logger.log(Errors::Syntax, "Prototype expected argument list '('.", lexer.peek_span());
    return nodei;
  }

  while (not_end(lexer, logger, ast, nodei, "prototype")) {
    auto t = lexer.peek_token();

    if (t.type == TokenType::RightParenthesis) {
      ast.nodes[nodei].span.absorb(t.span);
      lexer.consume_token();
      break;
    }

    if (t.type == TokenType::Name) {
      ast.nodes[nodei].span.absorb(t.span);
      lexer.consume_token();

      // expect type name
      auto nextt = lexer.peek_token();
      if (nextt.type != TokenType::Colon) {
        logger.log(Errors::Syntax, "Expected ':' then a type in argument list.", nextt.span);
        ast.nodes[nodei].malformed = true;
        continue;
      }
      lexer.consume_token();

      nextt = lexer.peek_token();
      if (nextt.type != TokenType::Name) {
        logger.log(Errors::Syntax, "Expected type after ':' in argument list.", nextt.span);
        ast.nodes[nodei].malformed = true;
        continue;
      }
      lexer.consume_token();

      data->args.emplace_back(ArgumentPrototype{ .name = t.value, .type = nextt.value });

      // consume commas between arguments
      nextt = lexer.peek_token();
      if (nextt.type == TokenType::Comma) {
        lexer.consume_token();
      } else if (nextt.type == TokenType::RightParenthesis) {
        continue;
      } else if (nextt.type == TokenType::Name) {
        logger.log(Errors::Syntax, "Missing comma between arguments.", nextt.span);
      }
    } else {
      // only report first error
      if (!ast.nodes[nodei].malformed)
        logger.log(Errors::Syntax, "Unexpected '" + to_string(t) + "' in argument list.", t.span);

      // if it's a semicolon, allow parsing to continue
      if (t.type == TokenType::Semicolon)
        break;

      lexer.consume_token();
      ast.nodes[nodei].malformed = true;
    }
  }

  if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer)) {
    logger.log(Errors::Syntax, "Expected ':' then return type after ')'.", lexer.peek_span());
    return nodei;
  }

  auto type = lexer.peek_token();
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer)) {
    logger.log(Errors::Syntax, "Expected return type after ':'.", lexer.peek_span());
    return nodei;
  }
  data->return_type = type.value;

  return nodei;
}

static int parse_block(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Block, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::LeftBrace, lexer))
    logger.log(Errors::Syntax, "Expected '{' to start code block.", lexer.peek_span());

  // @todo better understand unmatched braces
  while (not_end(lexer, logger, ast, nodei, "code block")) {
    if (lexer.peek_type() == TokenType::RightBrace) {
      ast.nodes[nodei].span.absorb(lexer.peek_span());
      lexer.consume_token();
      break;
    }

    add_child(ast, nodei, parse_statement(ast, lexer, logger));
  }

  return nodei;
}

static bool match_function_definition(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Name;
}

static int parse_function_definition(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::FunctionDefinition, lexer);

  add_child(ast, nodei, parse_prototype(ast, lexer, logger));

  int blocki = parse_block(ast, lexer, logger);
  ast.nodes[blocki].cast_data<ASTBlockData>()->llvm_bb_name = "entry";
  add_child(ast, nodei, blocki);

  return nodei;
}

static bool match_extern_function(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Extern;
}

static int parse_extern_function(AST& ast, Lexer& lexer, Logger& logger) {
  if (!expect_token(TokenType::Extern, lexer)) {
    logger.log(Errors::Syntax, "Expected 'extern'.", lexer.peek_span());
  }

  int nodei = parse_prototype(ast, lexer, logger);

  if (!expect_token(TokenType::Semicolon, lexer)) {
    logger.log(Errors::Syntax, "Expected ';'.", lexer.peek_span());
  }

  return nodei;
}

void parse_module_ast(AST& ast, std::string name, Lexer& lexer, Logger& logger) {
  int modulei = make_node(ast, ASTNodeType::Module, lexer);
  ast.nodes[modulei].cast_data<ASTModuleData>()->name = name;

  // parse top level statements
  while (true) {
    if (match_function_definition(lexer)) {
      add_child(ast, modulei, parse_function_definition(ast, lexer, logger));
    } else if (match_extern_function(lexer)) {
      add_child(ast, modulei, parse_extern_function(ast, lexer, logger));
    } else if (lexer.peek_type() == TokenType::Semicolon) {
      lexer.consume_token();
      continue;
    } else if (lexer.peek_type() == TokenType::End) {
      break;
    } else {
      lexer.consume_token();
      logger.log(Errors::Syntax, "Could not match top-level token.", lexer.peek_span());
    }
  }
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
