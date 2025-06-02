#include "parse_utils.hpp"
#include "lexer.hpp"
#include "logging.hpp"

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

bool expect_token(const TokenType& expected, Lexer& lexer, Logger& logger) {
  if (!expect_token(expected, lexer)) {
    logger.log(Errors::Syntax, "Expected " + to_string(expected) + ".", lexer.peek_span());
    return false;
  }
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

bool expect_token(ASTNode& node, const TokenType& expected, Lexer& lexer, Logger& logger, std::string message) {
  if (!expect_token(node, expected, lexer)) {
    if (message.empty())
      message = "Expected " + to_string(expected) + ".";
    logger.log(Errors::Syntax, message, node.span);
    return false;
  }
  return true;
}

void absorb_and_consume(AST& ast, int nodei, Lexer& lexer) {
  ast.nodes[nodei].span.absorb(lexer.peek_span());
  lexer.consume_token();
}

name_id add_name(AST& ast, int nodei, Lexer& lexer) {
  name_id id = ast.get_name_id(lexer.peek_value());
  ast.nodes[nodei].name = id;
  return id;
}

name_id get_name(AST& ast, int nodei) {
  LANG_ASSERT(ast.nodes[nodei].name >= 0);
  return ast.nodes[nodei].name;
}

bool not_end(Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context) {
  if (lexer.peek_type() == TokenType::End) {
    logger.log(Errors::Syntax, context != "" ? ("Unexpectedly reached end of file while parsing " + context + ".") : "Unexpectedly reached end of file.", ast.nodes[nodei].span);
    ast.nodes[nodei].malformed = true;
    return false;
  }

  return true;
}

bool not_end(Lexer& lexer, Logger& logger, std::string context) {
  if (lexer.peek_type() == TokenType::End) {
    logger.log(Errors::Syntax, context != "" ? ("Unexpectedly reached end of file while parsing " + context + ".") : "Unexpectedly reached end of file.", lexer.peek_span());
    return false;
  }

  return true;
}

bool not_matching(TokenType t, Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context) {
  if (not_end(lexer, logger, ast, nodei, context)) {
    if (lexer.matches(t)) {
      absorb_and_consume(ast, nodei, lexer);
      return false;
    }

    return true;
  }

  return false;
}

// @todo compelete spec following cpp
// https://en.cppreference.com/w/cpp/language/escape.html
std::string unescape_string(const std::string& input) {
  static const std::unordered_map<char, char> escape_map = {
      {'a', '\a'},  // Alert/bell
      {'b', '\b'},  // Backspace
      {'f', '\f'},  // Form feed
      {'n', '\n'},  // New line
      {'r', '\r'},  // Carriage return
      {'t', '\t'},  // Horizontal tab
      {'v', '\v'},  // Vertical tab
      {'\\', '\\'}, // Backslash
      {'\'', '\''}, // Single quote
      {'\"', '\"'}  // Double quote
  };

  std::string output;
  output.reserve(input.length()); // Pre-allocate memory
  
  bool escape_mode = false;
  
  for (size_t i = 0; i < input.size(); ++i) {
      if (escape_mode) {
          if (auto it = escape_map.find(input[i]); it != escape_map.end()) {
              output += it->second;
          }
          else if (input[i] == 'x' && i + 2 < input.size() && 
                  isxdigit(input[i+1]) && isxdigit(input[i+2])) {
              // Handle hex escapes (\xHH)
              char hex[3] = {input[i+1], input[i+2], '\0'};
              output += static_cast<char>(strtol(hex, nullptr, 16));
              i += 2;
          }
          else if (isdigit(input[i]) && i + 2 < input.size() && 
                  isdigit(input[i+1]) && isdigit(input[i+2])) {
              // Handle octal escapes (\OOO)
              char oct[4] = {input[i], input[i+1], input[i+2], '\0'};
              output += static_cast<char>(strtol(oct, nullptr, 8));
              i += 2;
          }
          else {
              // Invalid escape sequence - keep as is
              output += '\\';
              output += input[i];
          }
          escape_mode = false;
      }
      else if (input[i] == '\\') {
          escape_mode = true;
      }
      else {
          output += input[i];
      }
  }

  // Handle trailing backslash
  if (escape_mode) {
      output += '\\';
  }

  return output;
}