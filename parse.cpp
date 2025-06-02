#include "parse.hpp"
#include "parse_utils.hpp"
#include "parse_expression.hpp"
#include "parse_type.hpp"

static bool match_block(const Lexer& lexer);
static int parse_block(AST& ast, Lexer& lexer, Logger& logger, bool dont_create_basic_block = false);
static bool match_var_func_or_type(const Lexer& lexer);
static int parse_var_func_or_type(AST& ast, Lexer& lexer, Logger& logger);

static bool match_return_statement(const Lexer& lexer) {
  return lexer.matches(TokenType::Return);
}

static int parse_return_statement(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Return, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::Return, lexer)) {
    logger.log(Errors::Syntax, "Return expected.", lexer.peek_span());
    return nodei;
  }

  parse_expression_add_child(ast, lexer, logger, nodei);

  expect_token(ast.nodes[nodei], TokenType::Semicolon, lexer, logger);
  return nodei;
}

static bool match_if(const Lexer& lexer) {
  return lexer.matches(TokenType::If);
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
    if (lexer.matches(TokenType::Else, 0) && lexer.matches(TokenType::If, 1)) {
      absorb_and_consume(ast, nodei, lexer);
      absorb_and_consume(ast, nodei, lexer);

      parse_expression_add_child(ast, lexer, logger, nodei);

      int blocki = parse_block(ast, lexer, logger);
      ast.nodes[blocki].cast_data<ASTBlockData>()->llvm_bb_name = "elseif";
      add_child(ast, nodei, blocki);

      continue;
    }

    break;
  }

  data->has_else = lexer.matches(TokenType::Else);
  if (data->has_else) {
    lexer.consume_token();

    int blocki = parse_block(ast, lexer, logger);
    ast.nodes[blocki].cast_data<ASTBlockData>()->llvm_bb_name = "else";
    add_child(ast, nodei, blocki);
  }

  return nodei;
}

static bool match_while(const Lexer& lexer) {
  return lexer.matches(TokenType::While);
}

static int parse_while(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::While, lexer);

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

  if (!expect_token(ast.nodes[nodei], TokenType::Semicolon, lexer, logger)) {
    if (pos_before == lexer.peek_span().start.pos)
      lexer.consume_token();
  }

  return nodei;
}

static int parse_statement(AST& ast, Lexer& lexer, Logger& logger) {
  if (match_if(lexer)) {
    return parse_if(ast, lexer, logger);
  } else if (match_while(lexer)) {
    return parse_while(ast, lexer, logger);
  } else if (match_block(lexer)) {
    return parse_block(ast, lexer, logger, true);
  }

  int nodei;
  if (match_return_statement(lexer)) {
    nodei = parse_return_statement(ast, lexer, logger);
  } else if (match_var_func_or_type(lexer)) {
    nodei = parse_var_func_or_type(ast, lexer, logger);
  } else {
    nodei = parse_expression_statement(ast, lexer, logger);
  }

  return nodei;
}

static bool match_block(const Lexer& lexer) {
  return lexer.matches(TokenType::LeftBrace);
}

static int parse_block(AST& ast, Lexer& lexer, Logger& logger, bool dont_create_basic_block) {
  int nodei = make_node(ast, ASTNodeType::Block, lexer);

  ASTBlockData* data = ast.nodes[nodei].cast_data<ASTBlockData>();
  data->dont_create_basic_block = dont_create_basic_block;

  if (!expect_token(ast.nodes[nodei], TokenType::LeftBrace, lexer))
    logger.log(Errors::Syntax, "Expected '{' to start code block.", lexer.peek_span());

  // @todo better understand unmatched braces
  while (not_matching(TokenType::RightBrace, lexer, logger, ast, nodei, "code block")) {
    add_child(ast, nodei, parse_statement(ast, lexer, logger));
  }

  return nodei;
}

static int parse_generic_parameter_definition(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::GenericParameterDefinition, lexer);

  auto* data = ast.nodes[nodei].cast_data<ASTGenericParameterDefinitionData>();

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger, "Expected generic parameter name."))
    return nodei;
  
  static_assert((int)GenericParameterDefinitionChildren::Type == 1);
  data->has_type = lexer.matches(TokenType::Colon);
  if (data->has_type) {
    if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer, logger))
      return nodei;
      
    add_child(ast, nodei, parse_type(ast, lexer, logger));
  }

  data->has_default_value = lexer.matches(TokenType::Equals);
  if (data->has_default_value) {
    if (!expect_token(ast.nodes[nodei], TokenType::Equals, lexer, logger))
      return nodei;

    // the default value can either be a type or an expression,
    // to determine the difference we look for compile-time
    // directive. @note compile-time
    static_assert((int)GenericParameterDefinitionChildren::ExpressionOrType == 2);
    if (match_compile_time_directive(lexer)) {
      absorb_and_consume(ast, nodei, lexer);
      parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{.in_comma_list=true,.in_square=true});
    } else {
      add_child(ast, nodei, parse_type(ast, lexer, logger));
    }
  }

  return nodei;
}

static int parse_generic_parameter_definition_list(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::GenericParameterDefinitionList, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType::LeftSquare, lexer, logger))
    return nodei;

  while (not_matching(TokenType::RightSquare, lexer, logger, ast, nodei, "generic parameter definition list")) {
    add_child(ast, nodei, parse_generic_parameter_definition(ast, lexer, logger));

    if (!lexer.matches(TokenType::RightSquare)) {
      if (!expect_token(ast.nodes[nodei], TokenType::Comma, lexer, logger))
        break;
    }
  }

  return nodei;
}

static bool match_generic_parameter_definition_list(const Lexer& lexer) {
  return lexer.matches(TokenType::LeftSquare);
}

static bool match_var_func_or_type(const Lexer& lexer) {
  return 
    lexer.matches(TokenType::Name, 0) && 
    (lexer.matches(TokenType::Colon, 1) || lexer.matches(TokenType::LeftSquare, 1));
}

static int parse_var_func_or_type(AST& ast, Lexer& lexer, Logger& logger) {
  // we can't know without parsing further whether this is a variable definition
  // or a type definition
  int nodei = make_node(ast, ASTNodeType::Unknown, lexer);
  bool is_generic = false;
  bool has_type = false;

  add_name(ast, nodei, lexer);
  if (!expect_token(ast.nodes[nodei], TokenType::Name, lexer, logger))
    return nodei;

  if (match_generic_parameter_definition_list(lexer)) {
    static_assert((int)FunctionDeclarationChildren::GenericParameterDefinitionList == 1);
    static_assert((int)TypeDefinitionChildren::GenericParameterDefinitionList == 1);
    is_generic = true;

    add_child(ast, nodei, parse_generic_parameter_definition_list(ast, lexer, logger));
  }

  if (!expect_token(ast.nodes[nodei], TokenType::Colon, lexer, logger))
    return nodei;
  
  if (!lexer.matches(TokenType::Equals)) {
    has_type = true;

    static_assert((int)VariableDeclarationChildren::Type == 1);
    static_assert((int)FunctionDeclarationChildren::Type == 2);
    static_assert((int)TypeDefinitionChildren::Type == 2);
    add_child(ast, nodei, parse_type(ast, lexer, logger));
  }

  if (lexer.matches(TokenType::Equals)) {
    // this must be a variable or function declaration
    absorb_and_consume(ast, nodei, lexer);

    // @note assumes expression can't start with '{'
    if (lexer.matches(TokenType::LeftBrace)) {
      ast.nodes[nodei].type = ASTNodeType::FunctionDeclaration;
      construct_data(ast.nodes[nodei]);
      
      auto data = ast.nodes[nodei].cast_data<ASTFunctionDeclarationData>();
      data->is_generic = is_generic;
      data->has_type = has_type;
      
      static_assert((int)FunctionDeclarationChildren::Block == 3);
      add_child(ast, nodei, parse_block(ast, lexer, logger));
    } else {
      if (is_generic) {
        logger.log(Errors::Syntax, "A variable delaration cannot be generic.", ast.nodes[nodei].span);
        ast.nodes[nodei].malformed = true;
      }
      ast.nodes[nodei].type = ASTNodeType::VariableDeclaration;
      construct_data(ast.nodes[nodei]);
      
      static_assert((int)VariableDeclarationChildren::Expression == 2);
      parse_expression_add_child(ast, lexer, logger, nodei, ExpressionParsingContext{});

      expect_token(ast.nodes[nodei], TokenType::Semicolon, lexer, logger);
    }
  } else {
    // otherwise this is a type definition
    ast.nodes[nodei].type = ASTNodeType::TypeDefinition;
    construct_data(ast.nodes[nodei]);
    
    auto data = ast.nodes[nodei].cast_data<ASTTypeDefinitionData>();
    data->is_generic = is_generic;

    expect_token(ast.nodes[nodei], TokenType::Semicolon, lexer, logger);
  }

  return nodei;
}

void parse_module_ast(AST& ast, std::string name, Lexer& lexer, Logger& logger) {
  int modulei = make_node(ast, ASTNodeType::Module, lexer);

  // parse top level statements
  while (true) {
    if (match_var_func_or_type(lexer)) {
      add_child(ast, modulei, parse_var_func_or_type(ast, lexer, logger));
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