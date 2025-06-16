#include "parse_type.hpp"
#include "parse_expression.hpp"
#include "parse_utils.hpp"
#include "lexer.hpp"
#include "logging.hpp"

static GenericOrBuiltinType* parse_type_alias(AST& ast, Lexer& lexer, Logger& logger);
static StructDefinition* parse_struct_type(AST& ast, Lexer& lexer, Logger& logger);
static EnumDefinition* parse_enum_type(AST& ast, Lexer& lexer, Logger& logger);
static UnionDefinition* parse_union_type(AST& ast, Lexer& lexer, Logger& logger);
static FunctionDefinition* parse_function_type(AST& ast, Lexer& lexer, Logger& logger);

TypeExpression parse_type(AST& ast, Lexer& lexer, Logger& logger) {
  if (lexer.matches(TokenType::Name)) {
    return TypeExpression{.alias_or_primitive  = parse_type_alias(ast, lexer, logger)};
  } else if (lexer.matches(TokenType::Struct)) {
    return TypeExpression{.struct_definition   = parse_struct_type(ast, lexer, logger)};
  } else if (lexer.matches(TokenType::Enum)) {
    return TypeExpression{.enum_definition     = parse_enum_type(ast, lexer, logger)};
  } else if (lexer.matches(TokenType::Union)) {
    return TypeExpression{.union_definition    = parse_union_type(ast, lexer, logger)};
  } else if (lexer.matches(TokenType::LeftParenthesis)) {
    return TypeExpression{.function_definition = parse_function_type(ast, lexer, logger)};
  } else {
    return TypeExpression{.any                 = ast.create_node<ASTNode>(lexer.peek_span())};
  }
}

static GenericOrBuiltinType* parse_type_alias(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<GenericOrBuiltinType>(lexer.peek_span());

  node->reference = get_next_name(ast, lexer);
  if (!expect_token(node, TokenType::Name, lexer, logger))
    return node;

  if (match_generic_parameter_list(lexer))
    add_generic_parameter_list(node->first_parameter, node->num_parameters, node, ast, lexer, logger);

  return node;
}

bool add_generic_parameter_list(TypeOrValueExpression& first, size_t& num, ASTNode* node, AST& ast, Lexer& lexer, Logger& logger) {
  if (!expect_token(node, TokenType::LeftSquare, lexer, logger))
    return false;

  num = 0;
  while (not_matching(TokenType::RightSquare, lexer, logger, node)) {  
    TypeOrValueExpression parameter;

    if (match_compile_time_directive(lexer)) {
      absorb_and_consume(node, lexer);
      parameter.value = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_comma_list=true,.in_square=true});
    } else {
      parameter.type = parse_type(ast, lexer, logger);
    }

    if (num == 0)
      first = parameter;
    num++;

    if (!lexer.matches(TokenType::RightSquare)) {
      if (!expect_token(node, TokenType::Comma, lexer, logger))
        return false;
    }
  }

  return false;
}

bool match_generic_parameter_list(const Lexer& lexer) {
  return lexer.matches(TokenType::LeftSquare);
}

static StructEntry* parse_struct_entry(AST& ast, Lexer& lexer, Logger& logger);

static StructDefinition* parse_struct_type(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<StructDefinition>(lexer.peek_span());

  if (!expect_token(node, TokenType::Struct, lexer, logger))
    return node;

  // empty struct definition
  if (!lexer.matches(TokenType::LeftBrace)) {
    return node;
  } else {
    absorb_and_consume(node, lexer);
  }

  while (not_matching(TokenType::RightBrace, lexer, logger, node)) {
    auto entry = parse_struct_entry(ast, lexer, logger);
    if (node->first_entry == nullptr)
      node->first_entry = entry;

    add_child(node, entry);
    node->num_entries++;

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(node, TokenType::Comma, lexer, logger))
        return node;
    }
  }

  return node;
}

static StructEntry* parse_struct_entry(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<StructEntry>(lexer.peek_span());

  // spread operator
  node->is_spread = lexer.matches(TokenType::Ellipsis);
  if (node->is_spread) {
    if (!expect_token(node, TokenType::Ellipsis, lexer, logger))
      return node;

    node->entry_type = parse_type(ast, lexer, logger);
    add_child(node, node->entry_type.any);
    return node;
  }

  // or member
  node->member_name = get_next_name(ast, lexer);
  if (!expect_token(node, TokenType::Name, lexer, logger, 
    "Expected member name or spread operator in struct type definition."))
    return node;
  if (!expect_token(node, TokenType::Colon, lexer, logger))
    return node;

  if (!lexer.matches(TokenType::Equals)) {
    node->entry_type = parse_type(ast, lexer, logger);
    add_child(node, node->entry_type.any);
  }

  if (lexer.matches(TokenType::Equals)) {
    absorb_and_consume(node, lexer);

    node->default_value = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_comma_list=true});
    add_child(node, node->default_value.any);
  }

  return node;
}

static EnumEntry* parse_enum_entry(AST& ast, Lexer& lexer, Logger& logger);

static EnumDefinition* parse_enum_type(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<EnumDefinition>(lexer.peek_span());

  if (!expect_token(node, TokenType::Enum, lexer, logger))
    return node;

  if (lexer.matches(TokenType::LeftSquare)) {
    if (!expect_token(node, TokenType::LeftSquare, lexer, logger))
      return node;

    node->underlying_type = parse_type(ast, lexer, logger);
    add_child(node, node->underlying_type.any);
    
    if (!expect_token(node, TokenType::RightSquare, lexer, logger))
      return node;
  }

  // empty enum definition
  if (!lexer.matches(TokenType::LeftBrace)) {
    return node;
  } else {
    absorb_and_consume(node, lexer);
  }

  while (not_matching(TokenType::RightBrace, lexer, logger, node)) {
    auto entry = parse_enum_entry(ast, lexer, logger);
    if (node->first_entry == nullptr)
      node->first_entry = entry;
    add_child(node, entry);
    node->num_entries++;

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(node, TokenType::Comma, lexer, logger))
        return node;
    }
  }

  return node; 
}

static EnumEntry* parse_enum_entry(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<EnumEntry>(lexer.peek_span());

  // spread operator
  if (lexer.matches(TokenType::Ellipsis)) {
    if (!expect_token(node, TokenType::Ellipsis, lexer, logger))
      return node;

    node->spread_type = parse_type(ast, lexer, logger);
    add_child(node, node->spread_type.any);

    return node;
  }

  // or member
  node->constant_name = get_next_name(ast, lexer);
  if (!expect_token(node, TokenType::Name, lexer, logger, 
    "Expected member name or spread operator in enum type definition."))
    return node;

  if (lexer.matches(TokenType::Equals)) {
    absorb_and_consume(node, lexer);
    
    node->value = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_comma_list=true});
  }

  return node;
}

static UnionEntry* parse_union_entry(AST& ast, Lexer& lexer, Logger& logger);

static UnionDefinition* parse_union_type(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<UnionDefinition>(lexer.peek_span());

  if (!expect_token(node, TokenType::Union, lexer, logger))
    return node;

  // empty union definition
  if (!lexer.matches(TokenType::LeftBrace)) {
    return node;
  } else {
    absorb_and_consume(node, lexer);
  }

  while (not_matching(TokenType::RightBrace, lexer, logger, node)) {
    auto entry = parse_union_entry(ast, lexer, logger);
    if (node->first_entry == nullptr)
      node->first_entry = entry;
    add_child(node, entry);
    node->num_entries++;

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(node, TokenType::Comma, lexer, logger))
        return node;
    }
  }

  return node;
}

static UnionEntry* parse_union_entry(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<UnionEntry>(lexer.peek_span());

  node->participant_name = get_next_name(ast, lexer);
  if (!expect_token(node, TokenType::Name, lexer, logger, 
    "Expected member name or spread operator in union type definition."))
    return node;
  if (!expect_token(node, TokenType::Colon, lexer, logger))
    return node;

  node->entry_type = parse_type(ast, lexer, logger);
  add_child(node, node->entry_type.any);
  
  return node;
}

static bool add_parameter_definitions(FunctionParameterDefinition*& first, size_t& num, ASTNode* node, AST& ast, Lexer& lexer, Logger& logger);

static FunctionDefinition* parse_function_type(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<FunctionDefinition>(lexer.peek_span());

  add_parameter_definitions(node->first_parameter, node->num_parameters, node, ast, lexer, logger);

  if (!expect_token(node, TokenType::Arrow, lexer, logger,
      "Expected '->' followed by the function's return type."))
    return node;

  if (lexer.matches(TokenType::Void)) {
    absorb_and_consume(node, lexer);
    return node;
  }

  node->return_type = parse_type(ast, lexer, logger);
  add_child(node, node->return_type.any);
  return node;
}

static FunctionParameterDefinition* parse_parameter_definition(AST& ast, Lexer& lexer, Logger& logger);

static bool add_parameter_definitions(FunctionParameterDefinition*& first, size_t& num, ASTNode* node, AST& ast, Lexer& lexer, Logger& logger) {
  if (!expect_token(node, TokenType::LeftParenthesis, lexer, logger))
    return node;
  
  while (not_matching(TokenType::RightParenthesis, lexer, logger, node)) {
    auto parameter_definition = parse_parameter_definition(ast, lexer, logger);
    if (first == nullptr)
      first = parameter_definition;
    add_child(node, parameter_definition);
    num++;

    if (!lexer.matches(TokenType::RightParenthesis)) {
      if (!expect_token(node, TokenType::Comma, lexer, logger))
        return node;
    }
  }

  return node;
}

static FunctionParameterDefinition* parse_parameter_definition(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<FunctionParameterDefinition>(lexer.peek_span());

  node->identifier = get_next_name(ast, lexer);
  if (!expect_token(node, TokenType::Name, lexer, logger, 
    "Expected parameter name."))
    return node;
  if (!expect_token(node, TokenType::Colon, lexer, logger))
    return node;

  
  node->parameter_type = parse_type(ast, lexer, logger);
  add_child(node, node->parameter_type.any);

  if (lexer.matches(TokenType::Equals)) {
    absorb_and_consume(node, lexer);

    auto default_value = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_parenthesis=true,.in_comma_list=true});
    node->default_value = default_value;
    add_child(node, default_value.any);
  }

  return node;
}