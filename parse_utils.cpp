#include "parse_utils.hpp"

int make_node(AST& ast, ASTNodeType type, Lexer& lexer) {
  auto& node = ast.make_node(type);
  node.span = lexer.peek_span();
  return node.id;
}

bool expect_token(const TokenType& expected, Lexer& lexer) {
  if (lexer.peek_type() != expected) {
    return false;
  }

  lexer.consume_token();
  return true;
}

bool expect_token(ASTNode& node, const TokenType& expected, Lexer& lexer) {
  if (lexer.peek_type() != expected) {
    node.malformed = true;
    return false;
  }

  node.span.absorb(lexer.peek_span());
  lexer.consume_token();
  return true;
}

bool not_end(Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context) {
  if (lexer.peek_type() == TokenType::End) {
    logger.log(Errors::Syntax, context != "" ? ("Unexpectedly reached end of file while parsing " + context + ".") : "Unexpectedly reached end of file.", ast.nodes[nodei].span);
    ast.nodes[nodei].malformed = true;
    return false;
  }

  return true;
}
