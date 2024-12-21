#pragma once

#include "files.hpp"
#include <string>

enum class TokenType : char {
  Unknown = 0,
  End,
  Name,
  Return,
  Extern,
  Integer,
  Float,
};

struct Token {
  TokenType type = TokenType::Unknown;
  std::string value;
  TextSpan span;
};


std::string to_string(const TokenType& v);
std::string to_string(const Token& v);

class Lexer {
public:
  Lexer(Reader& reader, int src_id);

  TokenType peek_type(uint offset = 0) const;
  TextSpan peek_span(uint offset = 0) const;
  Token peek_token(uint offset = 0) const;

  void consume_token();

private:
  void get_token(Token& token);
  Reader& reader;
  int src_id;

  static const int MAX_LOOKAHEAD = 8;
  uint cur_token = 0;
  Token tokens[MAX_LOOKAHEAD];
};
