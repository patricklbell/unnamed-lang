#include <cstdio>
#include <iostream>
#include <string>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cctype>

#include "lexer.hpp"
#include "files.hpp"

Lexer::Lexer(Reader& _reader, int _src_id) : reader(_reader), src_id(_src_id) {
  for (int i = 0; i < MAX_LOOKAHEAD; ++i)
    get_token(tokens[i]);
}

void Lexer::consume_token() {
  uint prev_token = cur_token;
  cur_token = (cur_token + 1) % MAX_LOOKAHEAD;
  get_token(tokens[prev_token]);
}

TokenType Lexer::peek_type(uint offset) const {
  return tokens[(cur_token + offset) % MAX_LOOKAHEAD].type;
}

TextSpan Lexer::peek_span(uint offset) const {
  return tokens[(cur_token + offset) % MAX_LOOKAHEAD].span;
}

Token Lexer::peek_token(uint offset) const {
  return tokens[(cur_token + offset) % MAX_LOOKAHEAD];
}

void Lexer::get_token(Token& t) {
  static const int MAX_TOKEN_LEN = 16*1024;
  static char buf[MAX_TOKEN_LEN];

  while (true) {
    if (reader.eof()) {
      auto start = reader.get_peek_location();

      t.type = TokenType::End;
      t.span = TextSpan{ .start = start, .end = start, .src_id = src_id };
      return;
    }

    // consume spaces
    if (isspace(reader.peek_char())) {
      reader.consume_char();
      continue;
    }

    t.span.src_id = src_id;
    t.span.start = reader.get_peek_location();

    // [_a-zA-Z][a-zA-Z_0-9]
    if (isalpha(reader.peek_char()) || reader.peek_char() == '_') {
      int len = 0;

      do {
        buf[len++] = reader.consume_char();
      } while (isalnum(reader.peek_char()) || reader.peek_char() == '_');

      t.span.end = reader.get_consumed_location();
      t.value = std::string(buf, len);

      if (t.value == "return")
        t.type = TokenType::Return;
      else if (t.value == "extern")
        t.type = TokenType::Extern;
      else
        t.type = TokenType::Name;
      return;
    }

    // [0-9]*(\.[0-9]*)?f?
    if (isdigit(reader.peek_char()) || reader.peek_char() == '.') {
      bool is_float = reader.peek_char() == '.';

      int len = 0;
      // [0-9]*
      do {
        buf[len++] = reader.consume_char();
      } while (isdigit(reader.peek_char()));

      if (reader.peek_char() == 'f') {
        is_float = true;

        reader.consume_char();
      }
      else if (reader.peek_char() == '.') {
        is_float = true;

        // \.[0-9]*
        do {
          buf[len++] = reader.consume_char();
        } while (isdigit(reader.peek_char()));

        // consume extra 'f' if it is provided
        if (reader.peek_char() == 'f')
          reader.consume_char();
      }

      t.span.end = reader.get_consumed_location();
      t.type = is_float ? TokenType::Float : TokenType::Integer,
      t.value = std::string(buf, len);
      return;
    }

    // unmatched character
    {
      char c = reader.consume_char();

      t.span.end = reader.get_consumed_location();
      t.type = TokenType(c);
      return;
    }
  }
}

typedef std::underlying_type<TokenType>::type TokenTypeUnderlying;
std::string to_string(const TokenType& v) {
  switch (v) {
    case TokenType::Unknown:      return "Unknown";
    case TokenType::End:          return "End";
    case TokenType::Name:         return "Name";
    case TokenType::Return:       return "Return";
    case TokenType::Extern:       return "Extern";
    case TokenType::Integer:      return "Integer";
    case TokenType::Float:        return "Float";
  }

  return std::string(1, static_cast<TokenTypeUnderlying>(v));
}

std::string to_string(const Token& v) {
  return to_string(v.type);
}
