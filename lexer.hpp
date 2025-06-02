#pragma once

#include "files.hpp"

#include <string>
#include <array>

enum class TokenType : char {
  // unprintable characters (0-31)
  Unknown = 0,
  End,
  Name,
  Return,
  Extern,
  Integer,
  Float,
  String,
  If,
  Else,
  While,
  Struct,
  Enum,
  Union,
  Undefined,
  Void,
  LessEqual, // <=
  GreaterEqual, // >=
  DoubleEquals, // ==
  ExclamationEquals, // !=
  PlusEquals, // +=
  MinusEquals, // -=
  AsteriskEquals, // *=
  DivideEquals, // /=
  DoubleAnd, // &&
  DoublePipe, // ||
  DoublePlus, // ++
  DoubleMinus, // --
  Arrow, // ->
  Ellipsis, // ...

  // printable characters (32-)
  Tilde = '~',
  Dot = '.',
  Hash = '#',
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
  LeftSquare = '[',
  RightSquare = ']',
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
  std::string peek_value(uint offset = 0) const;
  bool matches(TokenType token, uint offset = 0) const;

  void consume_token();

  static const int MAX_LOOKAHEAD = 16;
  static const int NUM_TOKENS = MAX_LOOKAHEAD * 2;

private:
  void get_primitive_token(Token& token);
  void refresh_front_token_buffer();
  void refresh_back_token_buffer();

  Reader& reader;
  int src_id;

  uint cur_token;
  std::array<Token, NUM_TOKENS> tokens;

public:
  struct State {
    Reader::State reader_state;
    std::array<Token, NUM_TOKENS> tokens;
    uint cur_token = 0;
  };

  State save();
  void restore(const State& state);
};
