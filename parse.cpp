#include "ast.hpp"
#include "parse_utils.hpp"
#include "parse_expression.hpp"

static bool match_block(const Lexer& lexer);
static int parse_block(AST& ast, Lexer& lexer, Logger& logger, bool dont_create_basic_block = false);

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
  } else if (match_block(lexer)) {
    return parse_block(ast, lexer, logger, true);
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

static bool match_block(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::LeftBrace;
}

static int parse_block(AST& ast, Lexer& lexer, Logger& logger, bool dont_create_basic_block) {
  int nodei = make_node(ast, ASTNodeType::Block, lexer);

  ASTBlockData* data = ast.nodes[nodei].cast_data<ASTBlockData>();
  data->dont_create_basic_block = dont_create_basic_block;

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