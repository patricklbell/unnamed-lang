#include "parse_type.hpp"
#include "parse_expression.hpp"
#include "parse_utils.hpp"
#include "lexer.hpp"
#include "logging.hpp"

static int parse_type_alias(AST& ast, Lexer& lexer, Logger& logger);
static int parse_struct_type(AST& ast, Lexer& lexer, Logger& logger);
static int parse_enum_type(AST& ast, Lexer& lexer, Logger& logger);
static int parse_union_type(AST& ast, Lexer& lexer, Logger& logger);
static int parse_function_type(AST& ast, Lexer& lexer, Logger& logger);

int parse_type(AST& ast, Lexer& lexer, Logger& logger) {
  if (lexer.matches(TokenType::Name)) {
    return parse_type_alias(ast, lexer, logger);
  } else if (lexer.matches(TokenType::Struct)) {
    return parse_struct_type(ast, lexer, logger);
  } else if (lexer.matches(TokenType::Enum)) {
    return parse_enum_type(ast, lexer, logger);
  } else if (lexer.matches(TokenType::Union)) {
    return parse_union_type(ast, lexer, logger);
  } else if (lexer.matches(TokenType::LeftParenthesis)) {
    return parse_function_type(ast, lexer, logger);
  } else {
    return make_node(ast, ASTNodeType::Unknown, lexer);
  }
}

static int parse_type_alias(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::TypeAlias, lexer);
  auto data = ast.nodes[nodei].cast_data<ASTTypeAliasData>();

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger))
    return nodei;

  data->has_arguments = match_generic_parameter_list(lexer);
  if (!data->has_arguments) {
    return nodei;
  }

  add_child(ast, nodei, parse_generic_parameter_list(ast, lexer, logger));
  return nodei;
}

int parse_generic_parameter_list(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::GenericParameterList, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::LeftSquare, lexer, logger))
    return nodei;

  while (not_matching(TokenType::RightSquare, lexer, logger, ast, nodei, "generic parameter list")) {  
    if (match_compile_time_directive(lexer)) {
      static_assert((int)GenericParameterListChildren::Expression == -1);
      absorb_and_consume(ast, nodei, lexer);
      parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_comma_list=true,.in_square=true});
    } else {
      static_assert((int)GenericParameterListChildren::Type == -1);
      add_child(ast, nodei, parse_type(ast, lexer, logger));
    }

    if (!lexer.matches(TokenType::RightSquare)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger))
        return nodei;
    }
  }

  return nodei;
}

bool match_generic_parameter_list(const Lexer& lexer) {
  return lexer.matches(TokenType::LeftSquare);
}

static int parse_struct_entry(AST& ast, Lexer& lexer, Logger& logger);

static int parse_struct_type(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::StructDefinition, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::Struct, lexer, logger))
    return nodei;

  // empty struct definition
  if (!lexer.matches(TokenType::LeftBrace)) {
    return nodei;
  } else {
    absorb_and_consume(ast, nodei, lexer);
  }

  while (not_matching(TokenType::RightBrace, lexer, logger, ast, nodei, "struct definition list")) {
    static_assert((int)StructDefinitionChildren::StructEntry == -1);
    add_child(ast, nodei, parse_struct_entry(ast, lexer, logger));

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger))
        return nodei;
    }
  }

  return nodei;
}

static int parse_struct_entry(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::StructEntry, lexer);
  auto data = ast.nodes[nodei].cast_data<ASTStructEntryData>();

  // spread operator
  data->is_spread = lexer.matches(TokenType::Ellipsis);
  if (data->is_spread) {
    if (!expect_token(ast.nodes[nodei], TokenType::Ellipsis, lexer, logger))
      return nodei;

    data->is_spread = true;
    data->has_type = true;
    data->has_default_value = false;

    static_assert((int)StructEntryChildren::Type == 1);
    add_child(ast, nodei, parse_type(ast, lexer, logger));
    return nodei;
  }

  // or member
  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger, 
    "Expected member name or spread operator in struct type definition."))
    return nodei;
  if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer, logger))
    return nodei;

  data->has_type = !lexer.matches(TokenType::Equals);
  if (data->has_type) {
    static_assert((int)StructEntryChildren::Type == 1);
    add_child(ast, nodei, parse_type(ast, lexer, logger));
  }

  data->has_default_value = lexer.matches(TokenType::Equals);
  if (data->has_default_value) {
    if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer, logger))
      return nodei;

    static_assert((int)StructEntryChildren::Expression == 2);
    parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_comma_list=true});
  }

  return nodei;
}

static int parse_enum_entry(AST& ast, Lexer& lexer, Logger& logger);

static int parse_enum_type(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::EnumDefinition, lexer);
  auto data = ast.nodes[nodei].cast_data<ASTEnumDefinitionData>();

  if (!expect_token(ast.nodes[nodei], TokenType::Enum, lexer, logger))
    return nodei;

  data->has_type = lexer.matches(TokenType::LeftSquare);
  if (data->has_type) {
    if (!expect_token(ast.nodes[nodei], TokenType::LeftSquare, lexer, logger))
      return nodei;

    static_assert((int)EnumDefinitionChildren::Type == 1);
    add_child(ast, nodei, parse_type(ast, lexer, logger));
    
    if (!expect_token(ast.nodes[nodei], TokenType::RightSquare, lexer, logger))
      return nodei;
  }

  // empty enum definition
  if (!lexer.matches(TokenType::LeftBrace)) {
    return nodei;
  } else {
    absorb_and_consume(ast, nodei, lexer);
  }

  while (not_matching(TokenType::RightBrace, lexer, logger, ast, nodei, "enum definition list")) {
    static_assert((int)EnumDefinitionChildren::EnumEntry == -1);
    add_child(ast, nodei, parse_enum_entry(ast, lexer, logger));

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger))
        return nodei;
    }
  }

  return nodei; 
}

static int parse_enum_entry(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::EnumEntry, lexer);
  auto data = ast.nodes[nodei].cast_data<ASTEnumEntryData>();

  // spread operator
  data->is_spread = lexer.matches(TokenType::Ellipsis);
  if (data->is_spread) {
    if (!expect_token(ast.nodes[nodei], TokenType::Ellipsis, lexer, logger))
      return nodei;

    data->is_spread = true;
    data->has_value = false;

    static_assert((int)EnumEntryChildren::Type == 1);
    add_child(ast, nodei, parse_type(ast, lexer, logger));
    return nodei;
  }

  // or member
  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger, 
    "Expected member name or spread operator in enum type definition."))
    return nodei;

  data->has_value = lexer.matches(TokenType::Equals);
  if (data->has_value) {
    if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer, logger))
      return nodei;

    static_assert((int)EnumEntryChildren::Expression == 1);
    parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_comma_list=true});
  }

  return nodei;
}

static int parse_union_entry(AST& ast, Lexer& lexer, Logger& logger);

static int parse_union_type(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::UnionDefinition, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::Union, lexer, logger))
    return nodei;

  // empty union definition
  if (!lexer.matches(TokenType::LeftBrace)) {
    return nodei;
  } else {
    absorb_and_consume(ast, nodei, lexer);
  }

  while (not_matching(TokenType::RightBrace, lexer, logger, ast, nodei, "union definition list")) {
    static_assert((int)UnionDefinitionChildren::UnionEntry == -1);
    add_child(ast, nodei, parse_union_entry(ast, lexer, logger));

    if (!lexer.matches(TokenType::RightBrace)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger))
        return nodei;
    }
  }

  return nodei;
}

static int parse_union_entry(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::UnionEntry, lexer);
  auto data = ast.nodes[nodei].cast_data<ASTUnionEntryData>();

  // @todo not sure if spread is a good idea for unions
  data->is_spread = false;

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger, 
    "Expected member name or spread operator in union type definition."))
    return nodei;
  if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer, logger))
    return nodei;

  static_assert((int)UnionEntryChildren::Type == 1);
  add_child(ast, nodei, parse_type(ast, lexer, logger));
  
  return nodei;
}

static int parse_parameter_definition_list(AST& ast, Lexer& lexer, Logger& logger);

static int parse_function_type(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::FunctionDefinition, lexer);
  auto* data = ast.nodes[nodei].cast_data<ASTFunctionDefinitionData>();

  static_assert((int)FunctionDefinitionChildren::ParameterDefinitionList == 1);
  add_child(ast, nodei, parse_parameter_definition_list(ast, lexer, logger));

  if (!expect_token(ast.nodes[nodei], TokenType::Arrow, lexer, logger,
      "Expected '->' followed by the function's return type."))
    return nodei;

  if (lexer.matches(TokenType::Void)) {
    absorb_and_consume(ast, nodei, lexer);
    data->is_void = true;
    return nodei;
  }

  static_assert((int)FunctionDefinitionChildren::Type == 2);
  add_child(ast, nodei, parse_type(ast, lexer, logger));
  return nodei;
}

static int parse_parameter_definition(AST& ast, Lexer& lexer, Logger& logger);

static int parse_parameter_definition_list(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::ParameterDefinitionList, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::LeftParenthesis, lexer, logger))
    return nodei;
  
  while (not_matching(TokenType::RightParenthesis, lexer, logger, ast, nodei, "function parameter definition list")) {
    static_assert((int)UnionDefinitionChildren::UnionEntry == -1);
    add_child(ast, nodei, parse_parameter_definition(ast, lexer, logger));

    if (!lexer.matches(TokenType::RightParenthesis)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger))
        return nodei;
    }
  }

  return nodei;
}

static int parse_parameter_definition(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::ParameterDefinition, lexer);
  auto data = ast.nodes[nodei].cast_data<ASTParameterDefinitionData>();

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger, 
    "Expected parameter name."))
    return nodei;
  if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer, logger))
    return nodei;

  
  static_assert((int)StructEntryChildren::Type == 1);
  add_child(ast, nodei, parse_type(ast, lexer, logger));

  data->has_default_value = lexer.matches(TokenType::Equals);
  if (data->has_default_value) {
    if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer, logger))
      return nodei;

    static_assert((int)StructEntryChildren::Expression == 2);
    parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_parenthesis=true,.in_comma_list=true});
  }

  return nodei;
}