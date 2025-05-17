#pragma once

#include "files.hpp"
#include <string>

enum class TokenType : char {
  // unprintable characters (0-31)
  Unknown = 0,
  End,
  Name,
  Return,
  Extern,
  Integer,
  Float,
  If,
  Else,
  While,
  LessEqual, // <=
  GreaterEqual, // >=
  DoubleEquals, // ==
  DoubleAnd, // &&
  DoublePipe, // ||
  Walrus, // :=

  // printable characters (32-)
  Space = ' ',
  Plus = '+',
  Minus = '-',
  Asterisk = '*',
  ForwardSlash = '/',
  Modulo = '%',
  Greater = '>',
  Less = '<',
  And = '&',
  Pipe = '|',
  Exclamation = '!',
  Comma = ',',
  Semicolon = ';',
  Colon = ':',
  Equals = '=',
  LeftParenthesis = '(',
  RightParenthesis = ')',
  LeftBrace = '{',
  RightBrace = '}',
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
  void get_primitive_token(Token& token);
  void refresh_front_token_buffer();
  void refresh_back_token_buffer();

  Reader& reader;
  int src_id;

  static const int MAX_LOOKAHEAD = 8;
  static const int NUM_TOKENS = MAX_LOOKAHEAD * 2;
  uint cur_token = 0;
  Token tokens[NUM_TOKENS];
};
