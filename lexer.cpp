#include <cstdio>
#include <string>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cctype>

#include "lexer.hpp"
#include "files.hpp"

Lexer::Lexer(Reader& _reader, int _src_id) : reader(_reader), src_id(_src_id) {
  refresh_front_token_buffer();
  refresh_back_token_buffer();
}

void Lexer::consume_token() {
  cur_token = (cur_token + 1) % NUM_TOKENS;

  if (cur_token == 0)
    refresh_back_token_buffer();
  if (cur_token == MAX_LOOKAHEAD)
    refresh_front_token_buffer();
}

TokenType Lexer::peek_type(uint offset) const {
  return tokens[(cur_token + offset) % NUM_TOKENS].type;
}

TextSpan Lexer::peek_span(uint offset) const {
  return tokens[(cur_token + offset) % NUM_TOKENS].span;
}

Token Lexer::peek_token(uint offset) const {
  return tokens[(cur_token + offset) % NUM_TOKENS];
}

static bool try_merge(Token& t1, Token& t2) {
  bool merged = true;
  if (t1.type == TokenType::Less && t2.type == TokenType::Equals)
    t1.type = TokenType::LessEqual;
  else if (t1.type == TokenType::Greater && t2.type == TokenType::Equals)
    t1.type = TokenType::GreaterEqual;
  else if (t1.type == TokenType::Equals && t2.type == TokenType::Equals)
    t1.type = TokenType::DoubleEquals;
  else if (t1.type == TokenType::And && t2.type == TokenType::And)
    t1.type = TokenType::DoubleAnd;
  else if (t1.type == TokenType::Pipe && t2.type == TokenType::Pipe)
    t1.type = TokenType::DoublePipe;
  else if (t1.type == TokenType::Colon && t2.type == TokenType::Equals)
    t1.type = TokenType::Walrus;
  else
    merged = false;

  if (merged) {
    t1.value = t1.value + t2.value;
    t1.span.absorb(t2.span);
  }
  return merged;
}

// get tokens for 0 -> MAX_LOOKAHEAD - 1
void Lexer::refresh_front_token_buffer() {
  for (int i = 0; i < MAX_LOOKAHEAD; ++i) {
    get_primitive_token(tokens[i]);
    if (try_merge(tokens[(i - 1 + NUM_TOKENS) % NUM_TOKENS], tokens[i]))
      i--;
  }
}

// get tokens for MAX_LOOKAHEAD -> MAX_LOOKAHEAD*2 - 1
void Lexer::refresh_back_token_buffer() {
  for (int i = MAX_LOOKAHEAD; i < NUM_TOKENS; ++i) {
    get_primitive_token(tokens[i]);
    if (try_merge(tokens[(i - 1 + NUM_TOKENS) % NUM_TOKENS], tokens[i]))
      i--;
  }
}

void Lexer::get_primitive_token(Token& t) {
  static const int MAX_TOKEN_LEN = 16*1024;
  static char buf[MAX_TOKEN_LEN];

  bool in_inline_comment = false;
  bool in_block_comment = false;

  while (true) {
    if (reader.eof()) {
      auto start = reader.get_peek_location();

      t.type = TokenType::End;
      t.span = TextSpan{ .start = start, .end = start, .src_id = src_id };
      return;
    }

    // consume inline comments
    if (in_inline_comment) {
      if (reader.consume_char() == '\n') {
        in_inline_comment = false;
      } else {
        continue;
      }
    }

    // consume block comments
    if (in_block_comment) {
      if (reader.consume_char() == '*' && reader.peek_char() == '/') {
        reader.consume_char();
        in_block_comment = false;
      } else {
        continue;
      }
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
      else if (t.value == "if")
        t.type = TokenType::If;
      else if (t.value == "else")
        t.type = TokenType::Else;
      else if (t.value == "while")
        t.type = TokenType::While;
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

      if (c == '/') {
        if (reader.peek_char() == '*') {
          reader.consume_char();
          in_block_comment = true;
          continue;
        } else if (reader.peek_char() == '/') {
          reader.consume_char();
          in_inline_comment = true;
          continue;
        }
      }

      t.span.end = reader.get_consumed_location();
      t.type = TokenType(c);
      t.value = std::string(1, c);
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
    case TokenType::If:           return "If";
    case TokenType::Else:         return "Else";
    case TokenType::While:        return "While";
    default:
      return std::to_string(static_cast<TokenTypeUnderlying>(v));
  }
}

std::string to_string(const Token& v) {
  return  "'" + v.value + "' (" + to_string(v.type) + ")";
}
