#include "parse.hpp"
#include "parse_utils.hpp"
#include "parse_expression.hpp"
#include "parse_type.hpp"

static bool match_block(const Lexer& lexer);
static Block* parse_block(AST& ast, Lexer& lexer, Logger& logger, bool dont_create_basic_block = false);
static bool match_var_func_or_type(const Lexer& lexer);
static ASTNode* parse_var_func_or_type(AST& ast, Lexer& lexer, Logger& logger);

static bool match_return_statement(const Lexer& lexer) {
  return lexer.matches(TokenType::Return);
}

static ReturnStatement* parse_return_statement(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<ReturnStatement>(lexer.peek_span());

  if (!expect_token(node, TokenType::Return, lexer)) {
    logger.log(Errors::Syntax, "Return expected.", lexer.peek_span());
    return node;
  }

  node->expression = parse_expression(ast, lexer, logger);
  add_child(node, node->expression.any);

  expect_token(node, TokenType::Semicolon, lexer, logger);
  return node;
}

static bool match_if(const Lexer& lexer) {
  return lexer.matches(TokenType::If);
}

static IfElseStatement* parse_if(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<IfElseStatement>(lexer.peek_span());

  if (!expect_token(node, TokenType::If, lexer)) {
    logger.log(Errors::Syntax, "If expected.", lexer.peek_span());
    return node;
  }

  node->first_condition = parse_expression(ast, lexer, logger);
  add_child(node, node->first_condition.any);

  auto if_block = parse_block(ast, lexer, logger);
  if_block->llvm_bb_name = "if";
  add_child(node, if_block);

  while (not_end(lexer, logger, node)) {
    if (lexer.matches(TokenType::Else, 0) && lexer.matches(TokenType::If, 1)) {
      absorb_and_consume(node, lexer);
      absorb_and_consume(node, lexer);

      add_child(node, parse_expression(ast, lexer, logger).any);

      auto block = parse_block(ast, lexer, logger);
      block->llvm_bb_name = "elseif";
      add_child(node, block);

      continue;
    }

    break;
  }

  if (lexer.matches(TokenType::Else)) {
    lexer.consume_token();

    node->else_block = parse_block(ast, lexer, logger);
    node->else_block->llvm_bb_name = "else";
    add_child(node, node->else_block);
  }

  return node;
}

static bool match_while(const Lexer& lexer) {
  return lexer.matches(TokenType::While);
}

static WhileStatement* parse_while(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<WhileStatement>(lexer.peek_span());

  if (!expect_token(node, TokenType::While, lexer)) {
    logger.log(Errors::Syntax, "While expected.", lexer.peek_span());
    return node;
  }

  node->condition = parse_expression(ast, lexer, logger);
  add_child(node, node->condition.any);

  node->body = parse_block(ast, lexer, logger);
  add_child(node, node->body);

  return node;
}

static ExpressionStatement* parse_expression_statement(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<ExpressionStatement>(lexer.peek_span());

  // make sure expression consumes some tokens so that parse doesn't get stuck on blocks
  int pos_before = lexer.peek_span().start.pos;

  node->expression = parse_expression(ast, lexer, logger);
  add_child(node, node->expression.any);

  if (!expect_token(node, TokenType::Semicolon, lexer, logger)) {
    if (pos_before == lexer.peek_span().start.pos)
      lexer.consume_token();
  }

  return node;
}

static ASTNode* parse_statement(AST& ast, Lexer& lexer, Logger& logger) {
  if (match_if(lexer)) {
    return parse_if(ast, lexer, logger);
  } else if (match_while(lexer)) {
    return parse_while(ast, lexer, logger);
  } else if (match_block(lexer)) {
    return parse_block(ast, lexer, logger, true);
  } else if (match_return_statement(lexer)) {
    return parse_return_statement(ast, lexer, logger);
  } else if (match_var_func_or_type(lexer)) {
    return parse_var_func_or_type(ast, lexer, logger);
  }

  return parse_expression_statement(ast, lexer, logger);
}

static bool match_block(const Lexer& lexer) {
  return lexer.matches(TokenType::LeftBrace);
}

static Block* parse_block(AST& ast, Lexer& lexer, Logger& logger, bool dont_create_basic_block) {
  auto node = ast.create_node<Block>(lexer.peek_span());

  node->dont_create_basic_block = dont_create_basic_block;

  if (!expect_token(node, TokenType::LeftBrace, lexer))
    logger.log(Errors::Syntax, "Expected '{' to start code block.", lexer.peek_span());

  // @todo better understand unmatched braces
  while (not_matching(TokenType::RightBrace, lexer, logger, node)) {
    auto statement = parse_statement(ast, lexer, logger);
    if (node->first_statement_or_scope == nullptr)
      node->first_statement_or_scope = statement;

    add_child(node, statement);
    node->num_statements_or_scopes++;
  }

  return node;
}

static GenericParameterDefinition* parse_generic_parameter_definition(AST& ast, Lexer& lexer, Logger& logger) {
  auto node = ast.create_node<GenericParameterDefinition>(lexer.peek_span());

  node->parameter_identifier = get_next_name(ast, lexer);
  if (!expect_token(node, TokenType::Name, lexer, logger, "Expected generic parameter name."))
    return node;
  
  if (lexer.matches(TokenType::Colon)) {
    if (!expect_token(node, TokenType::Colon, lexer, logger))
      return node;
      
    node->parameter_type = parse_type(ast, lexer, logger);
    add_child(node, node->parameter_type.any);
  }

  if (lexer.matches(TokenType::Equals)) {
    absorb_and_consume(node, lexer);

    // the default value can either be a type or an expression,
    // to determine the difference we look for compile-time
    // directive. @note compile-time
    TypeOrValueExpression default_value;
    if (match_compile_time_directive(lexer)) {
      absorb_and_consume(node, lexer); // @todo assumes compile time directive is one token
      default_value.value = parse_expression(ast, lexer, logger, ExpressionParsingContext{.in_comma_list=true,.in_square=true});
    } else {
      default_value.type = parse_type(ast, lexer, logger);
    }
    add_child(node, default_value.any);
    node->default_value = default_value;
  }

  return node;
}

static bool match_generic_parameter_definition_list(const Lexer& lexer) {
  return lexer.matches(TokenType::LeftSquare);
}

static bool match_var_func_or_type(const Lexer& lexer) {
  return 
    lexer.matches(TokenType::Name, 0) && 
    (lexer.matches(TokenType::Colon, 1) || lexer.matches(TokenType::LeftSquare, 1));
}

// @note can return a null ptr if it fails
static ASTNode* parse_var_func_or_type(AST& ast, Lexer& lexer, Logger& logger) {
  TextSpan start_span = lexer.peek_span();
  // we can't know without parsing further whether this is a variable definition
  // or a type definition
  name_id identifier = get_next_name(ast, lexer);
  expect_token(TokenType::Name, lexer, logger);

  std::vector<GenericParameterDefinition*> generic_parameters;
  if (match_generic_parameter_definition_list(lexer)) {
    expect_token(TokenType::LeftSquare, lexer, logger);

    while (not_matching(TokenType::RightSquare, lexer, logger)) {
      generic_parameters.push_back(parse_generic_parameter_definition(ast, lexer, logger));
      
      if (!lexer.matches(TokenType::RightSquare)) {
        expect_token(TokenType::Comma, lexer, logger);
      }
    }
  }

  expect_token(TokenType::Colon, lexer, logger);
  
  TypeExpression type = TypeExpression{.any = nullptr};
  if (!lexer.matches(TokenType::Equals)) {
    type = parse_type(ast, lexer, logger);
  }

  ASTNode* node;
  if (lexer.matches(TokenType::Equals)) {
    // this must be a variable or function declaration
    lexer.consume_token();

    // @note assumes expression can't start with '{'
    if (lexer.matches(TokenType::LeftBrace)) {
      auto function_declaration = ast.create_node<FunctionDeclaration>(start_span);
      node = function_declaration;
      
      function_declaration->identifier = identifier;
      function_declaration->num_generic_parameters = generic_parameters.size();
      function_declaration->first_generic_parameter = generic_parameters.empty() ? nullptr : generic_parameters[0];
      for (auto p : generic_parameters) add_child(function_declaration, p);
      function_declaration->definition = type;
      if (function_declaration->definition.any != nullptr)
        add_child(function_declaration, function_declaration->definition.any);
      function_declaration->body = parse_block(ast, lexer, logger);
      add_child(function_declaration, function_declaration->body);
    } else {
      auto variable_declaration = ast.create_node<VariableDeclaration>(start_span);
      node = variable_declaration;

      if (!generic_parameters.empty()) {
        logger.log(Errors::Syntax, "A variable delaration cannot be generic.", node->span);
        variable_declaration->malformed = true;
      }
      
      variable_declaration->identifier = identifier;
      variable_declaration->desired_type = type;
      if (variable_declaration->desired_type.any != nullptr)
        add_child(variable_declaration, variable_declaration->desired_type.any);
      variable_declaration->value = parse_expression(ast, lexer, logger);
      add_child(variable_declaration, variable_declaration->value.any);

      expect_token(variable_declaration, TokenType::Semicolon, lexer, logger);
    }
  } else {
    auto type_definition = ast.create_node<TypeDefinition>(start_span);
    node = type_definition;

    type_definition->identifier = identifier;
    type_definition->num_generic_parameters = generic_parameters.size();
    type_definition->first_generic_parameter = generic_parameters.empty() ? nullptr : generic_parameters[0];
    for (auto p : generic_parameters) add_child(type_definition, p); 
    type_definition->type_expression = type;
    if (type_definition->type_expression.any != nullptr)
      add_child(type_definition, type_definition->type_expression.any);

    expect_token(node, TokenType::Semicolon, lexer, logger);
  }
  node->span.absorb(start_span);

  return node;
}

void parse_module_ast(AST& ast, std::string name, Lexer& lexer, Logger& logger) {
  auto module = ast.create_node<Module>(lexer.peek_span());
  module->module_name = ast.get_name_id(name);
  add_child_from_different_source(&ast.root, module);

  // parse top level statements
  while (true) {
    if (match_var_func_or_type(lexer)) {
      auto node = parse_var_func_or_type(ast, lexer, logger);
      if (module->first_top_level_declaration == nullptr)
        module->first_top_level_declaration = node;
      module->num_top_level_declaration++;
      add_child(module, node);
    } else if (lexer.matches(TokenType::Semicolon)) {
      lexer.consume_token();
      continue;
    } else if (lexer.matches(TokenType::End)) {
      break;
    } else {
      lexer.consume_token();
      logger.log(Errors::Syntax, "Could not match top-level token.", lexer.peek_span());
    }
  }
}