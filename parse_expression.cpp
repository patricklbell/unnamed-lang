#include "parse_expression.hpp"
#include "parse_utils.hpp"

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

void parse_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, int parent_precedence, bool in_parenthesis, bool in_comma_list) {
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