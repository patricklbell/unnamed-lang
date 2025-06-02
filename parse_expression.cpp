#include "parse_expression.hpp"
#include "parse_utils.hpp"
#include "parse_type.hpp"
#include "lexer.hpp"
#include "logging.hpp"
#include "ast.hpp"

static bool match_end_expression(const Lexer& lexer, ExpressionParsingContext settings) {
  return (
    (settings.in_comma_list && lexer.peek_type() == TokenType::Comma)             ||
    (settings.in_parenthesis && lexer.peek_type() == TokenType::RightParenthesis) ||
    (settings.in_square && lexer.peek_type() == TokenType::RightSquare)           ||
    lexer.peek_type() == TokenType::Semicolon                            ||
    lexer.peek_type() == TokenType::LeftBrace                            ||
    lexer.peek_type() == TokenType::RightBrace                           ||
    lexer.peek_type() == TokenType::End
  );
}

static int parse_variable(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Variable, lexer);
  
  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger))
    return nodei;

  return nodei;
}

static bool match_variable(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Name;
}

static int parse_literal(AST& ast, Lexer& lexer, Logger& logger) {
  auto t = lexer.peek_token();

  int nodei = -1;
  if (t.type == TokenType::Integer) {
    nodei = make_node(ast, ASTNodeType::IntLiteral, lexer);
    auto& node = ast.nodes[nodei];

    node.cast_data<ASTIntLiteralData>()->value = std::stoi(t.value);
  }
  else if (t.type == TokenType::Float) {
    nodei = make_node(ast, ASTNodeType::FloatLiteral, lexer);
    auto& node = ast.nodes[nodei];

    node.cast_data<ASTFloatLiteralData>()->value = std::stof(t.value);
  }
  else if (t.type == TokenType::String) {
    nodei = make_node(ast, ASTNodeType::StringLiteral, lexer);
    auto& node = ast.nodes[nodei];

    if (t.value.length() >= 2 && t.value[0] == '"' && t.value[t.value.length() - 1] == '"') {
      node.cast_data<ASTStringLiteralData>()->value = unescape_string(t.value.substr(1, t.value.length() - 2));
    } else {
      logger.log(Errors::Syntax, "Invalid string literal.", t.span);
    }
  }
  else if (t.type == TokenType::Undefined) {
    nodei = make_node(ast, ASTNodeType::UndefinedLiteral, lexer);
  }

  absorb_and_consume(ast, nodei, lexer);
  
  LANG_ASSERT(nodei > 0, "Missing parsing for matched literal");
  return nodei;
}

static bool match_literal(const Lexer& lexer) {
  return (
    lexer.peek_type() == TokenType::Float ||
    lexer.peek_type() == TokenType::Integer ||
    lexer.peek_type() == TokenType::String
  );
}

// simple call means not a function pointer i.e. expression
static bool match_generic_or_simple_call(Lexer& lexer) {
  if (!lexer.matches(TokenType::Name, 0))
    return false;
  // non-generic call
  if (lexer.matches(TokenType::LeftParenthesis, 1))
    return true;

  // otherwise, we need to check if it is a generic
  if (!lexer.matches(TokenType::LeftSquare, 1))
    return false;

  // we need to consume the contents of the square brackets to
  // contextualise whether this is a generic or a subscript
  Lexer::State state = lexer.save();

  lexer.consume_token();
  lexer.consume_token();
  
  // @note assumes that there are no unmatched square brackets inside
  int square_brace_count = 1;
  while (!lexer.matches(TokenType::End) && square_brace_count > 0) {
    if (lexer.matches(TokenType::LeftSquare)) {
      square_brace_count++;
    } else if (lexer.matches(TokenType::RightSquare)) {
      square_brace_count--;
    }
    lexer.consume_token();
  }

  bool matches = lexer.matches(TokenType::LeftParenthesis);

  lexer.restore(state);
  return matches;
}

static int parse_generic_or_simple_call(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Call, lexer);
  auto* data = ast.nodes[nodei].cast_data<ASTCallData>();
  data->is_function_pointer = false;

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger))
    return nodei;

  data->is_generic = match_generic_parameter_list(lexer);
  if (data->is_generic) {
    static_assert((int)CallChildren::GenericParameterList == 1);
    add_child(ast, nodei, parse_generic_parameter_list(ast, lexer, logger));
  }
  
  if (!expect_token(ast.nodes[nodei], TokenType::LeftParenthesis, lexer, logger)) 
    return nodei;

  ExpressionParsingContext list_settings {
    .in_parenthesis = true,
    .in_comma_list = true,
    .in_square = false,
  };

  while (not_matching(TokenType::RightParenthesis, lexer, logger, ast, nodei, "call")) {
    static_assert((int)CallChildren::Expression == -1);
    parse_expression_add_child(ast, lexer, logger, nodei, list_settings);

    if (!lexer.matches(TokenType::RightParenthesis)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger, 
                   "Expected ',' between arguments."))
        return nodei;
    }
  }

  return nodei;
}

static int parse_struct_literal_entry_list(AST& ast, Lexer& lexer, Logger& logger);

static int parse_struct_literal(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::StructLiteral, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::Struct, lexer, logger)) 
    return nodei;

  static_assert((int)StructLiteralChildren::Type == 1);
  add_child(ast, nodei, parse_type(ast, lexer, logger));

  static_assert((int)StructLiteralChildren::StructLiteralEntryList == 2);
  add_child(ast, nodei, parse_struct_literal_entry_list(ast, lexer, logger));

  return nodei;
}

static bool match_struct_literal(const Lexer& lexer) {
  return lexer.matches(TokenType::Struct);
}

static int parse_struct_literal_entry(AST& ast, Lexer& lexer, Logger& logger);

static int parse_struct_literal_entry_list(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::StructLiteralEntryList, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::LeftBrace, lexer, logger)) 
    return nodei;

  while (not_matching(TokenType::RightBrace, lexer, logger, ast, nodei, "struct literal")) {
    static_assert((int)StructLiteralEntryListChildren::StructLiteralEntry == -1);
    add_child(ast, nodei, parse_struct_literal_entry(ast, lexer, logger));

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger, 
                        "Expected ',' between struct literal entries."))
        return nodei;
    }
  }

  return nodei;
}

static int parse_struct_literal_entry(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::StructLiteralEntry, lexer);

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger)) 
    return nodei;
  if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer, logger)) 
    return nodei;
  
  static_assert((int)StructLiteralEntryChildren::Expression == 1);
  parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_comma_list=true});

  return nodei;
}

static bool match_subscript_postfix(Lexer& lexer) {
  return lexer.matches(TokenType::LeftSquare);
}

// @note assumes highest precedence i.e. always makes itself parent
static int parse_subscript_postfix(AST& ast, Lexer& lexer, Logger& logger, int lhsi) {
  int nodei = make_parent(ast, ASTNodeType::BinaryOperator, lhsi); 
  ASTBinaryOperatorData* data = ast.nodes[nodei].cast_data<ASTBinaryOperatorData>();
  data->op = BinaryOperator::Subscript;
  
  if (!expect_token(ast.nodes[nodei], TokenType::LeftSquare, lexer, logger))
    return nodei;
  parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_square=true});
  if (!expect_token(ast.nodes[nodei], TokenType::RightSquare, lexer, logger))
    return nodei;

  {
    // correct the size from tree editing after adding a child
    auto& lhs = ast.nodes[nodei + 1];
    auto& rhs = ast.nodes[lhs.id + lhs.size];
    ast.nodes[nodei].size = 1 + lhs.size + rhs.size;
  }

  return nodei;
}

static bool match_function_pointer_call_postfix(Lexer& lexer) {
  return lexer.matches(TokenType::LeftParenthesis);
}

// @note assumes highest precedence i.e. always makes itself parent
static int parse_function_pointer_call_postfix(AST& ast, Lexer& lexer, Logger& logger, int lhsi) {
  int nodei = make_parent(ast, ASTNodeType::Call, lhsi); 
  ASTCallData* data = ast.nodes[nodei].cast_data<ASTCallData>();
  data->is_function_pointer = true;
  data->is_generic = false;
  
  if (!expect_token(ast.nodes[nodei], TokenType::LeftParenthesis, lexer, logger))
    return nodei;

  ExpressionParsingContext list_settings {
    .in_parenthesis = true,
    .in_comma_list = true,
    .in_square = false,
  };

  while (not_matching(TokenType::RightParenthesis, lexer, logger, ast, nodei, "call")) {
    static_assert((int)CallChildren::Expression == -1);
    parse_expression_add_child(ast, lexer, logger, nodei, list_settings);

    if (!lexer.matches(TokenType::RightParenthesis)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger, 
                   "Expected ',' between arguments."))
        return nodei;
    }
  }

  {
    // correct the size from tree editing after adding children
    int size = 1;
    for (const auto& child : ASTChildrenIterator(ast, nodei)) {
      size += child.size;
    }
  }

  return nodei;
}

static void parse_parenthesis_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti) {
  TextSpan maybe_left_parenthesis_span = lexer.peek_span();
  bool has_left_parenthesis = expect_token(TokenType::LeftParenthesis, lexer);

  if (!has_left_parenthesis)
    logger.log(Errors::Syntax, "Expected '('.", lexer.peek_span());

  ExpressionParsingContext child_settings {
    .in_parenthesis = has_left_parenthesis,
    .in_comma_list = false,
    .in_square = false,
  };
  parse_expression_add_child(ast, lexer, logger, parenti, child_settings);

  if (has_left_parenthesis && !expect_token(TokenType::RightParenthesis, lexer))
    logger.log(Errors::Syntax, "Unmatched parenthesis.", maybe_left_parenthesis_span);
}

static bool match_parenthesis_expression(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::LeftParenthesis;
}

static void parse_expression_atom_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, ExpressionParsingContext settings) {
  if (match_literal(lexer)) {
    add_child(ast, parenti, parse_literal(ast, lexer, logger));
  } else if (match_parenthesis_expression(lexer)) {
    parse_parenthesis_expression_add_child(ast, lexer, logger, parenti);
  } else if (match_generic_or_simple_call(lexer)) {
    add_child(ast, parenti, parse_generic_or_simple_call(ast, lexer, logger));
  } else if (match_struct_literal(lexer)) {
    add_child(ast, parenti, parse_struct_literal(ast, lexer, logger));
  } else if (match_variable(lexer)) {
    add_child(ast, parenti, parse_variable(ast, lexer, logger));
  } else if (match_end_expression(lexer, settings)) {
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
  {TokenType::DoubleEquals,     {7,   BinaryOperator::Equal}},
  {TokenType::ExclamationEquals,{7,   BinaryOperator::NotEqual}},
  {TokenType::DoubleAnd,        {11,  BinaryOperator::LogicalAnd}},
  {TokenType::DoublePipe,       {12,  BinaryOperator::LogicalOr}},
  {TokenType::Equals,           {14,  BinaryOperator::Assign}},
  {TokenType::PlusEquals,       {14,  BinaryOperator::PlusAssign}},
  {TokenType::MinusEquals,      {14,  BinaryOperator::MinusAssign}},
  {TokenType::AsteriskEquals,   {14,  BinaryOperator::MultiplyAssign}},
  {TokenType::DivideEquals,     {14,  BinaryOperator::DivideAssign}},
};

// @note assumes LHS has been parsed
static int parse_binary_operator(AST& ast, Lexer& lexer, Logger& logger, int lhsi, int& parent_precedence, ExpressionParsingContext settings) {
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
  absorb_and_consume(ast, nodei, lexer);

  // parse the rhs greedily
  parse_expression_add_child(ast, lexer, logger, nodei, settings, precedence);

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

static std::unordered_map<TokenType, UnaryOperator> prefix_unary_operator_table = {
  {TokenType::DoublePlus,       UnaryOperator::PrefixIncrement},
  {TokenType::DoubleMinus,      UnaryOperator::PrefixDecrement},
  {TokenType::Plus,             UnaryOperator::Plus},
  {TokenType::Minus,            UnaryOperator::Minus},
  {TokenType::Exclamation,      UnaryOperator::LogicalNot},
  {TokenType::Asterisk,         UnaryOperator::Dereference},
  {TokenType::And,              UnaryOperator::AddressOf},
};

static bool match_prefix_unary_operator(Lexer& lexer) {
  return prefix_unary_operator_table.find(lexer.peek_type()) != prefix_unary_operator_table.end();
}

static int parse_prefix_unary_operator(AST& ast, Lexer& lexer, Logger& logger, int parenti, ExpressionParsingContext settings) {
  int nodei = make_node(ast, ASTNodeType::UnaryOperator, lexer);
  ASTUnaryOperatorData* data = ast.nodes[nodei].cast_data<ASTUnaryOperatorData>();

  data->op = prefix_unary_operator_table.find(lexer.peek_type())->second;
  absorb_and_consume(ast, nodei, lexer);

  // @note precedence is right-to-left
  if (match_prefix_unary_operator(lexer))
    // allow multiple unary operators eg. !!x
    add_child(ast, nodei, parse_prefix_unary_operator(ast, lexer, logger, nodei, settings));
  else
    parse_expression_atom_add_child(ast, lexer, logger, nodei, settings);

  return nodei;
}

// @todo efficiency
static bool match_cast_operator(Lexer& lexer) {
  if (!lexer.matches(TokenType::LeftParenthesis))
    return false;
  
  auto state = lexer.save();

  int parenthesis_count = 1;
  lexer.consume_token();
  while (parenthesis_count > 0) {
    if (lexer.matches(TokenType::LeftParenthesis)) {
      parenthesis_count++;
    } else if (lexer.matches(TokenType::RightParenthesis)) {
      parenthesis_count--;
    }
    lexer.consume_token();
  }

  // check that after closing parenthesis is a name or a prefix operator
  bool is_match = lexer.matches(TokenType::Name) || match_prefix_unary_operator(lexer);

  lexer.restore(state);

  return is_match;
}

static int parse_cast_operator(AST& ast, Lexer& lexer, Logger& logger, int parenti, ExpressionParsingContext settings) {
  int nodei = make_node(ast, ASTNodeType::CastOperator, lexer);
  
  if (!expect_token(ast.nodes[nodei], TokenType::LeftParenthesis, lexer, logger))
    return nodei;
  
  static_assert((int)CastOperatorChildren::Type == 1);
  add_child(ast, nodei, parse_type(ast, lexer, logger));

  if (!expect_token(ast.nodes[nodei], TokenType::RightParenthesis, lexer, logger))
    return nodei;

  static_assert((int)CastOperatorChildren::Expression == 2);
  parse_expression_atom_add_child(ast, lexer, logger, nodei, settings);

  return nodei;
}

static std::unordered_map<TokenType, UnaryOperator> postfix_unary_operator_table = {
  {TokenType::DoublePlus,       UnaryOperator::PostfixIncrement},
  {TokenType::DoubleMinus,      UnaryOperator::PostfixDecrement},
  {TokenType::Dot,              UnaryOperator::MemberAccess},
  {TokenType::Arrow,            UnaryOperator::PointerMemberAccess},
};

static bool match_postfix_unary_operator(Lexer& lexer) {
  return postfix_unary_operator_table.find(lexer.peek_type()) != postfix_unary_operator_table.end();
}

// @note assumes highest precedence i.e. always makes itself parent
static int parse_postfix_unary_operation(AST& ast, Lexer& lexer, Logger& logger, int childi) {
  int nodei = make_parent(ast, ASTNodeType::UnaryOperator, childi);
  ASTUnaryOperatorData* data = ast.nodes[nodei].cast_data<ASTUnaryOperatorData>();

  data->op = postfix_unary_operator_table.find(lexer.peek_type())->second;
  absorb_and_consume(ast, nodei, lexer);

  if (
    data->op == UnaryOperator::MemberAccess || 
    data->op == UnaryOperator::PointerMemberAccess
  ) {
    add_name(ast, nodei, lexer);
    expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger, "Expected member name.");
  }

  return nodei;
}

void parse_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, ExpressionParsingContext settings, int parent_precedence) {
  int parent_original_size = ast.nodes[parenti].size;

  // add at least one atom and use this as the lhs for any binary expressions
  int lhsi = ast.nodes[parenti].id + ast.nodes[parenti].size;
  if (match_cast_operator(lexer)) {
    add_child(ast, parenti, parse_cast_operator(ast, lexer, logger, parenti, settings));
  } else if (match_prefix_unary_operator(lexer)) {
    add_child(ast, parenti, parse_prefix_unary_operator(ast, lexer, logger, parenti, settings));
  } else {
    parse_expression_atom_add_child(ast, lexer, logger, parenti, settings);
  }

  // greedily capture operators, replacing the parent as we go
  while (true) {
    // @note left-to-right precedence, postfix higher than all binary
    if (match_postfix_unary_operator(lexer))
      // @note always makes itself parent
      lhsi = parse_postfix_unary_operation(ast, lexer, logger, lhsi);
    // special postfix cases
    else if (match_subscript_postfix(lexer))
      lhsi = parse_subscript_postfix(ast, lexer, logger, lhsi);
    else if (match_function_pointer_call_postfix(lexer))
      lhsi = parse_function_pointer_call_postfix(ast, lexer, logger, lhsi);
    else if (match_binary_operator(lexer)) // @note c precedence has assignment operator right-to-left
      lhsi = parse_binary_operator(ast, lexer, logger, lhsi, parent_precedence, settings);
    else
      break;
  }

  // @note this will require separators between expression
  if (!match_end_expression(lexer, settings)) {
    logger.log(Errors::Syntax, "Expected end of expression.", lexer.peek_span());
  }
  // recover from failing to match expression end
  while (!match_end_expression(lexer, settings)) {
    ast.nodes[parenti].span.absorb(lexer.peek_span());
    lexer.consume_token();
  }

  // since we added the child and then modified the tree, the size 
  // may be wrong and needs to be corrected
  ast.nodes[parenti].size = parent_original_size + ast.nodes[lhsi].size;
  ast.nodes[parenti].span.absorb(ast.nodes[lhsi].span);
}

bool match_compile_time_directive(const Lexer& lexer) {
  return lexer.matches(TokenType::Hash);
}