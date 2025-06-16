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
    lexer.peek_type() == TokenType::Semicolon                                     ||
    lexer.peek_type() == TokenType::LeftBrace                                     ||
    lexer.peek_type() == TokenType::RightBrace                                    ||
    lexer.peek_type() == TokenType::End
  );
}

static ValueExpression parse_variable(AST& ast, Lexer& lexer, Logger& logger) {
  auto variable = ast.create_node<Variable>(lexer.peek_span());
  
  variable->identifier = get_next_name(ast, lexer);
  if (!expect_token(variable, TokenType::Name, lexer, logger))
    return ValueExpression{.variable = variable};

  return ValueExpression{.variable = variable};
}

static bool match_variable(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Name;
}

static ValueExpression parse_literal(AST& ast, Lexer& lexer, Logger& logger) {
  auto t = lexer.peek_token();

  ValueExpression value;
  if (t.type == TokenType::Integer) {
    auto int_ = ast.create_node<IntLiteral>(lexer.peek_span());
    value.int_ = int_;

    int_->value = std::stoi(t.value);
  }
  else if (t.type == TokenType::Float) {
    auto float_ = ast.create_node<FloatLiteral>(lexer.peek_span());
    value.float_ = float_;

    float_->value = std::stof(t.value);
  }
  else if (t.type == TokenType::String) {
    auto string = ast.create_node<StringLiteral>(lexer.peek_span());
    value.string = string;

    if (t.value.length() >= 2 && t.value[0] == '"' && t.value[t.value.length() - 1] == '"') {
      string->value = unescape_string(t.value.substr(1, t.value.length() - 2));
    } else {
      logger.log(Errors::Syntax, "Invalid string literal.", t.span);
    }
  }
  else if (t.type == TokenType::Undefined) {
    auto undefined = ast.create_node<UndefinedLiteral>(lexer.peek_span());
    value.undefined = undefined;
  } else {
    LANG_ASSERT(false, "Missing parsing for matched literal");
  }

  absorb_and_consume(value.any, lexer);
  
  return value;
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

static bool add_function_parameters(Call* call, AST& ast, Lexer& lexer, Logger& logger) {
  static ExpressionParsingContext list_settings {
    .in_parenthesis = true,
    .in_comma_list = true,
    .in_square = false,
  };

  if (!expect_token(call, TokenType::LeftParenthesis, lexer, logger)) 
    return false;

  call->first_parameter.any = nullptr;
  while (not_matching(TokenType::RightParenthesis, lexer, logger, call)) {
    ValueExpression parameter = parse_expression(ast, lexer, logger, list_settings);
    if (call->first_parameter.any == nullptr)
      call->first_parameter = parameter;

    add_child(call, parameter.any);
    call->num_parameters++;

    if (!lexer.matches(TokenType::RightParenthesis)) {
      if (!expect_token(call, TokenType::Comma, lexer, logger, 
                   "Expected ',' between arguments."))
        return false;
    }
  }

  return true;
}

static ValueExpression parse_generic_or_simple_call(AST& ast, Lexer& lexer, Logger& logger) {
  auto call = ast.create_node<Call>(lexer.peek_span());

  call->function_name = get_next_name(ast, lexer);
  if (!expect_token(call, TokenType::Name, lexer, logger))
    return ValueExpression{.call = call};

  if (match_generic_parameter_list(lexer))
    add_generic_parameter_list(call->first_generic_parameter, call->num_generic_parameters, call, ast, lexer, logger);
  
  add_function_parameters(call, ast, lexer, logger);

  return ValueExpression{.call = call};
}

static ValueExpression parse_struct_literal_entry_list(AST& ast, Lexer& lexer, Logger& logger);

static ValueExpression parse_struct_literal(AST& ast, Lexer& lexer, Logger& logger) {
  auto struct_ = ast.create_node<StructLiteral>(lexer.peek_span());

  if (!expect_token(struct_, TokenType::Struct, lexer, logger)) 
    return ValueExpression{.struct_ = struct_};

  struct_->referencing_type = parse_type(ast, lexer, logger);
  add_child(struct_, struct_->referencing_type.any);

  if (!expect_token(struct_, TokenType::LeftBrace, lexer, logger)) 
    return ValueExpression{.struct_ = struct_};

  struct_->first_entry.any = nullptr;
  while (not_matching(TokenType::RightBrace, lexer, logger, struct_)) {
    struct_->entry_member_names.emplace_back(get_next_name(ast, lexer));

    if (!expect_token(struct_, TokenType::Name, lexer, logger)) 
      return ValueExpression{.struct_=struct_};
    if (!expect_token(struct_, TokenType::Equals, lexer, logger)) 
      return ValueExpression{.struct_=struct_};

    ValueExpression value = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_comma_list=true});
    if (struct_->first_entry.any == nullptr)
      struct_->first_entry = value;
    add_child(struct_, value.any);
    struct_->num_entries++;

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(struct_, TokenType::Comma, lexer, logger, 
                        "Expected ',' between struct literal entries."))
        return ValueExpression{.struct_ = struct_};
    }
  }

  return ValueExpression{.struct_ = struct_};
}

static bool match_struct_literal(const Lexer& lexer) {
  return lexer.matches(TokenType::Struct);
}

static bool match_subscript_postfix(Lexer& lexer) {
  return lexer.matches(TokenType::LeftSquare);
}

// @note assumes highest precedence i.e. always makes itself parent
static ValueExpression parse_subscript_postfix(AST& ast, Lexer& lexer, Logger& logger, ValueExpression lhs) {
  auto binary = ast.create_node<BinaryOperation>(lexer.peek_span());
  add_parent(lhs.any, binary);
  binary->lhs = lhs;
  binary->op = BinaryOperator::Subscript;
  
  if (!expect_token(binary, TokenType::LeftSquare, lexer, logger))
    return ValueExpression{.binary = binary};

  binary->rhs = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_square=true});
  add_child(binary, binary->rhs.any);

  if (!expect_token(binary, TokenType::RightSquare, lexer, logger))
    return ValueExpression{.binary = binary};

  return ValueExpression{.binary = binary};
}

static bool match_function_pointer_call_postfix(Lexer& lexer) {
  return lexer.matches(TokenType::LeftParenthesis);
}

// @note assumes highest precedence i.e. always makes itself parent
static ValueExpression parse_function_pointer_call_postfix(AST& ast, Lexer& lexer, Logger& logger, ValueExpression lhs) {
  auto call = ast.create_node<Call>(lexer.peek_span());
  add_parent(lhs.any, call);
  call->function_pointer = lhs;

  add_function_parameters(call, ast, lexer, logger);

  return ValueExpression{.call = call};
}

static ValueExpression parse_parenthesis_expression(AST& ast, Lexer& lexer, Logger& logger) {
  TextSpan maybe_left_parenthesis_span = lexer.peek_span();
  bool has_left_parenthesis = expect_token(TokenType::LeftParenthesis, lexer);

  if (!has_left_parenthesis)
    logger.log(Errors::Syntax, "Expected '('.", lexer.peek_span());

  ExpressionParsingContext child_settings {
    .in_parenthesis = has_left_parenthesis,
    .in_comma_list = false,
    .in_square = false,
  };
  auto value = parse_expression(ast, lexer, logger, child_settings);

  if (has_left_parenthesis && !expect_token(TokenType::RightParenthesis, lexer))
    logger.log(Errors::Syntax, "Unmatched parenthesis.", maybe_left_parenthesis_span);

  return value;
}

static bool match_parenthesis_expression(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::LeftParenthesis;
}

static ValueExpression parse_expression_atom(AST& ast, Lexer& lexer, Logger& logger, ExpressionParsingContext settings) {
  if (match_literal(lexer)) {
    return parse_literal(ast, lexer, logger);
  } else if (match_parenthesis_expression(lexer)) {
    return parse_parenthesis_expression(ast, lexer, logger);
  } else if (match_generic_or_simple_call(lexer)) {
    return parse_generic_or_simple_call(ast, lexer, logger);
  } else if (match_struct_literal(lexer)) {
    return parse_struct_literal(ast, lexer, logger);
  } else if (match_variable(lexer)) {
    return parse_variable(ast, lexer, logger);
  } else if (match_end_expression(lexer, settings)) {
    logger.log(Errors::Syntax, "Unexpected end of expression.", lexer.peek_span());
    return ValueExpression{.any = ast.create_node<ASTNode>(lexer.peek_span())};
  } else {
    logger.log(Errors::Syntax, "Unexpected token in expression.", lexer.peek_span());
    return ValueExpression{.any = ast.create_node<ASTNode>(lexer.peek_span())};
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
static ValueExpression parse_binary_operator(AST& ast, Lexer& lexer, Logger& logger, ValueExpression lhs, int& parent_precedence, ExpressionParsingContext settings) {
  auto precedence_binop = binary_operator_precedence_table.find(lexer.peek_type())->second;

  // if the precedence of the operator is lower then return the parent
  // if it's equal then return (left to right associativity)
  int precedence = precedence_binop.first; 
  if (parent_precedence != -1 && precedence >= parent_precedence) {
    parent_precedence = -1;
    // @note since parent_precedence should be -1 at top level,
    // this should never exit the expression i.e. any is allowed here
    return ValueExpression{.any = lhs.any->parent};
  }

  // create a parent for the lhs
  auto binary = ast.create_node<BinaryOperation>(lexer.peek_span());
  add_parent(lhs.any, binary);
  binary->lhs = lhs;

  binary->op = precedence_binop.second;
  absorb_and_consume(binary, lexer);

  // parse the rhs greedily
  binary->rhs = parse_expression(ast, lexer, logger, settings, precedence);
  add_child(binary, binary->rhs.any);

  return ValueExpression{.binary = binary};
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

static ValueExpression parse_prefix_unary_operator(AST& ast, Lexer& lexer, Logger& logger, ExpressionParsingContext settings) {
  auto unary = ast.create_node<UnaryOperation>(lexer.peek_span());
  
  unary->op = prefix_unary_operator_table.find(lexer.peek_type())->second;
  absorb_and_consume(unary, lexer);

  // @note precedence is right-to-left
  if (match_prefix_unary_operator(lexer)) {
    // allow multiple unary operators eg. !!x
    unary->operand = parse_prefix_unary_operator(ast, lexer, logger, settings);
  } else {
    unary->operand = parse_expression_atom(ast, lexer, logger, settings);
  }

  add_child(unary, unary->operand.any);

  return ValueExpression{.unary = unary};
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

static ValueExpression parse_cast_operator(AST& ast, Lexer& lexer, Logger& logger, ExpressionParsingContext settings) {
  auto cast = ast.create_node<CastOperation>(lexer.peek_span());
  
  if (!expect_token(cast, TokenType::LeftParenthesis, lexer, logger))
    return ValueExpression{ .cast = cast };
  
  cast->desired_type = parse_type(ast, lexer, logger);
  add_child(cast, cast->desired_type.any);

  if (!expect_token(cast, TokenType::RightParenthesis, lexer, logger))
    return ValueExpression{ .cast = cast };

  cast->value = parse_expression_atom(ast, lexer, logger, settings);
  add_child(cast, cast->value.any);

  return ValueExpression{ .cast = cast };
}

static std::unordered_map<TokenType, UnaryOperator> postfix_unary_operator_table = {
  {TokenType::DoublePlus,       UnaryOperator::PostfixIncrement},
  {TokenType::DoubleMinus,      UnaryOperator::PostfixDecrement},
  {TokenType::Dot,              UnaryOperator::Member},
  {TokenType::Arrow,            UnaryOperator::PointerMember},
};

static bool match_postfix_unary_operator(Lexer& lexer) {
  return postfix_unary_operator_table.find(lexer.peek_type()) != postfix_unary_operator_table.end();
}

// @note assumes highest precedence i.e. always makes itself parent
static ValueExpression parse_postfix_unary_operation(AST& ast, Lexer& lexer, Logger& logger, ValueExpression child) {
  UnaryOperator op = postfix_unary_operator_table.find(lexer.peek_type())->second;
  TextSpan span = lexer.peek_span();
  lexer.consume_token();

  ValueExpression value;
  if (op == UnaryOperator::Member) {
    auto opr = ast.create_node<MemberOperation>(lexer.peek_span());
    opr->reference = child;
    opr->member = get_next_name(ast, lexer);
    expect_token(opr, TokenType::Name, lexer, logger, "Expected member name.");
    value.member = opr;
  } else if (op == UnaryOperator::PointerMember) {
    auto opr = ast.create_node<PointerMemberOperation>(lexer.peek_span());
    opr->pointer = child;
    opr->member = get_next_name(ast, lexer);
    expect_token(opr, TokenType::Name, lexer, logger, "Expected member name.");
    value.pointer_member = opr;
  } else {
    auto opr = ast.create_node<UnaryOperation>(lexer.peek_span());
    opr->op = op;
    opr->operand = child;
    value.unary = opr;
  }

  add_parent(child.any, value.any);
  value.any->span = span;
  return value;
}

ValueExpression parse_expression(AST& ast, Lexer& lexer, Logger& logger, ExpressionParsingContext settings, int parent_precedence) {
  // add at least one atom and use this as the lhs for any binary expressions
  ValueExpression lhs;
  if (match_cast_operator(lexer)) {
    lhs = parse_cast_operator(ast, lexer, logger, settings);
  } else if (match_prefix_unary_operator(lexer)) {
    lhs = parse_prefix_unary_operator(ast, lexer, logger, settings);
  } else {
    lhs = parse_expression_atom(ast, lexer, logger, settings);
  }

  // greedily capture operators, replacing the parent as we go
  while (true) {
    // @note left-to-right precedence, postfix higher than all binary
    if (match_postfix_unary_operator(lexer))
      // @note always makes itself parent
      lhs = parse_postfix_unary_operation(ast, lexer, logger, lhs);
    // special postfix cases
    else if (match_subscript_postfix(lexer))
      lhs = parse_subscript_postfix(ast, lexer, logger, lhs);
    else if (match_function_pointer_call_postfix(lexer))
      lhs = parse_function_pointer_call_postfix(ast, lexer, logger, lhs);
    else if (match_binary_operator(lexer)) // @note c precedence has assignment operator right-to-left
      lhs = parse_binary_operator(ast, lexer, logger, lhs, parent_precedence, settings);
    else
      break;
  }

  // @note this will require separators between expression
  if (!match_end_expression(lexer, settings)) {
    logger.log(Errors::Syntax, "Expected end of expression.", lexer.peek_span());
  }
  // recover from failing to match expression end
  while (!match_end_expression(lexer, settings)) {
    lhs.any->span.absorb(lexer.peek_span());
    lexer.consume_token();
  }

  return lhs;
}

bool match_compile_time_directive(const Lexer& lexer) {
  return lexer.matches(TokenType::Hash);
}