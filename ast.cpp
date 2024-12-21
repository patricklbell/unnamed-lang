#include "ast.hpp"
#include "files.hpp"
#include "logging.hpp"
#include "lexer.hpp"
#include <iostream>
#include <string>
#include <vector>

// @todo avoid allocating unkown data twice using class constructor
ASTNode::ASTNode(ASTNodeType _type) : data(unique_void(new ASTUnkownData{})) {
  type = _type;
  switch(type) {
    case ASTNodeType::Unknown:
       malformed = true;
       data = unique_void(new ASTUnkownData{});
       return;  
    case ASTNodeType::Assignment:
       data = unique_void(new ASTAssignmentData{});
       return;
    case ASTNodeType::Return:
       data = unique_void(new ASTReturnData{});
       return;
    case ASTNodeType::Literal:
       data = unique_void(new ASTLiteralData{});
       return;
    case ASTNodeType::Call:
       data = unique_void(new ASTCallData{});
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

ASTNode& AST::make_node(ASTNodeType type) {
  auto& node = nodes.emplace_back(type);
  node.id = nodes.size() - 1;
  return node;
}

// Helpers
static void add_child(AST& ast, int parent, int child) {
  ast.nodes[parent].size += ast.nodes[child].size;
  ast.nodes[parent].num_children++;
  ast.nodes[child].parent = ast.nodes[parent].id;
  ast.nodes[parent].span.absorb(ast.nodes[child].span);
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

static bool not_end(Lexer& lexer, Logger& logger, ASTNode& node, std::string context = "") {
  if (lexer.peek_type() == TokenType::End) {
    logger.log(Errors::Syntax, context != "" ? ("Unexpectedly reached end of file while parsing " + context + ".") : "Unexpectedly reached end of file.", node.span);
    node.malformed = true;
    return false;
  }

  return true;
}

// Parsing
static int parse_literal(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Literal, lexer);

  {
    auto t = lexer.peek_token();
    lexer.consume_token();

    if (!(t.type == TokenType::Float || t.type == TokenType::Integer)) {
      logger.log(Errors::Syntax, "Expected float or integer literal, not '" + to_string(t.type) + "'.", t.span);
      ast.nodes[nodei].malformed = true;
      return nodei;
    }

    ast.nodes[nodei].span.absorb(t.span);
    ast.nodes[nodei].data = unique_void(new ASTLiteralData{ .value = std::stof(t.value) });
  }

  return nodei;
}

static bool match_literal(const Lexer& lexer) {
  return (
    lexer.peek_type() == TokenType::Float ||
    lexer.peek_type() == TokenType::Integer
  );
}

static int parse_expression(AST& ast, Lexer& lexer, Logger& logger);

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

  if (!expect_token(ast.nodes[nodei], TokenType('('), lexer)) {
    logger.log(Errors::Syntax, "Call expected argument list '('.", lexer.peek_span());
    return nodei;
  }

  while (not_end(lexer, logger, ast.nodes[nodei], "call")) {
    if (lexer.peek_type() == TokenType(')')) {
      ast.nodes[nodei].span.absorb(lexer.peek_span());
      lexer.consume_token();
      break;
    }

    add_child(ast, nodei, parse_expression(ast, lexer, logger));

    auto nextt = lexer.peek_type();
    if (nextt == TokenType(',')) {
      lexer.consume_token();
      continue;
    }
    if (nextt == TokenType(')')) {
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
    lexer.peek_type(1) == TokenType('(')
  );
}

static int parse_expression(AST& ast, Lexer& lexer, Logger& logger) {
  if (match_literal(lexer))
    return parse_literal(ast, lexer, logger);
  else if (match_call(lexer))
    return parse_call(ast, lexer, logger);
  else {
    logger.log(Errors::Syntax, "Could not understand token in expression.", lexer.peek_span());
    lexer.consume_token();
    return make_node(ast, ASTNodeType::Unknown, lexer);
  }
}

static bool match_assignment(const Lexer& lexer) {
  return (
    lexer.peek_type(0) == TokenType::Name &&
    lexer.peek_type(1) == TokenType('=')
  );
}

static int parse_assignment(AST& ast, Lexer& lexer, Logger& logger) {
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

  if (!expect_token(ast.nodes[nodei], TokenType('='), lexer)) {
    logger.log(Errors::Syntax, "Assignment expected '='.", lexer.peek_span());
    return nodei;
  }

  add_child(ast, nodei, parse_expression(ast, lexer, logger));

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

  add_child(ast, nodei, parse_expression(ast, lexer, logger));

  return nodei;
}

static int parse_statement(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei;
  if (match_return(lexer))
    nodei = parse_return(ast, lexer, logger);
  else if (match_assignment(lexer))
    nodei = parse_assignment(ast, lexer, logger);
  else
    nodei = parse_expression(ast, lexer, logger);

  if (!expect_token(ast.nodes[nodei], TokenType(';'), lexer))
    logger.log(Errors::Syntax, "Missing semi-colon at the end of statement.", lexer.peek_span());

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

  if (!expect_token(ast.nodes[nodei], TokenType('('), lexer)) {
    logger.log(Errors::Syntax, "Prototype expected argument list '('.", lexer.peek_span());
    return nodei;
  }

  while (not_end(lexer, logger, ast.nodes[nodei], "prototype")) {
    auto t = lexer.peek_token();

    if (t.type == TokenType(')')) {
      ast.nodes[nodei].span.absorb(t.span);
      lexer.consume_token();
      break;
    }

    if (t.type == TokenType::Name) {
      ast.nodes[nodei].span.absorb(t.span);
      lexer.consume_token();

      data->args.emplace_back(ArgumentPrototype{ .name = t.value });

      // consume commas between arguments
      auto nextt = lexer.peek_token();
      if (nextt.type == TokenType(',')) {
        lexer.consume_token();
      } else if (nextt.type == TokenType(')')) {
        continue;
      } else if (nextt.type == TokenType::Name) {
        logger.log(Errors::Syntax, "Missing comma between arguments.", nextt.span);
      }
    } else {
      // only report first error
      if (!ast.nodes[nodei].malformed)
        logger.log(Errors::Syntax, "Unexpected '" + to_string(t.type) + "' in argument list.", t.span);

      // if it's a semicolon, allow parsing to continue
      if (t.type == TokenType(';'))
        break;

      lexer.consume_token();
      ast.nodes[nodei].malformed = true;
    }
  }

  return nodei;
}

static int parse_block(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::Block, lexer);

  if (!expect_token(ast.nodes[nodei], TokenType('{'), lexer)) {
    logger.log(Errors::Syntax, "Expected '{' to start code block.", lexer.peek_span());
    return nodei;
  }

  // @todo better understand unmatched close brace
  while (not_end(lexer, logger, ast.nodes[nodei], "code block")) {
    auto t = lexer.peek_token();

    if (t.type == TokenType('}')) {
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

static int parse_function_defintion(AST& ast, Lexer& lexer, Logger& logger) {
  int nodei = make_node(ast, ASTNodeType::FunctionDefinition, lexer);

  add_child(ast, nodei, parse_prototype(ast, lexer, logger));

  add_child(ast, nodei, parse_block(ast, lexer, logger));

  return nodei;
}

static bool match_function_prototype(const Lexer& lexer) {
  return lexer.peek_type() == TokenType::Extern;
}

static int parse_function_prototype(AST& ast, Lexer& lexer, Logger& logger) {
  if (!expect_token(TokenType::Extern, lexer)) {
    logger.log(Errors::Syntax, "Expected 'extern'.", lexer.peek_span());
  }

  return parse_prototype(ast, lexer, logger);
}

void make_ast(AST& ast, Lexer& lexer, Logger& logger) {
  int modulei = make_node(ast, ASTNodeType::Module, lexer);

  // parse top level statements
  while (true) {
    if (match_function_definition(lexer)) {
      add_child(ast, modulei, parse_function_defintion(ast, lexer, logger));
    } else if (match_function_prototype(lexer)) {
      add_child(ast, modulei, parse_function_prototype(ast, lexer, logger));
    } else if (lexer.peek_type() == TokenType(';')) {
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

std::string to_string(const ASTNodeType& type) {
  switch (type) {
    case ASTNodeType::Unknown:                return "Unknown";
    case ASTNodeType::Assignment:             return "Assignment";
    case ASTNodeType::Return:                 return "Return";
    case ASTNodeType::Call:                   return "Call";
    case ASTNodeType::Literal:                return "Literal";
    case ASTNodeType::Prototype:              return "Prototype";
    case ASTNodeType::Block:                  return "Block";
    case ASTNodeType::FunctionDefinition:     return "FunctionDefinition";
    case ASTNodeType::Module:                 return "Module";
  }
}

std::string to_string(const ASTNode& node) {
  std::string res = to_string(node.type);

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
