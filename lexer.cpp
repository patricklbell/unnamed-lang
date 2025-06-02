#include <cstdio>
#include <string>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cctype>

#include "lexer.hpp"
#include "files.hpp"

Lexer::Lexer(Reader& _reader, int _src_id) : reader(_reader), src_id(_src_id), cur_token(0) {
  refresh_front_token_buffer();
  refresh_back_token_buffer();
}

static inline int circular_lu(int i) {
  return (i + Lexer::NUM_TOKENS) % Lexer::NUM_TOKENS;
}

void Lexer::consume_token() {
  cur_token = circular_lu(cur_token + 1);

  if (cur_token == 0)
    refresh_back_token_buffer();
  if (cur_token == MAX_LOOKAHEAD)
    refresh_front_token_buffer();
}

TokenType Lexer::peek_type(uint offset) const {
  return tokens[circular_lu(cur_token + offset)].type;
}

TextSpan Lexer::peek_span(uint offset) const {
  return tokens[circular_lu(cur_token + offset)].span;
}

std::string Lexer::peek_value(uint offset) const {
  return tokens[circular_lu(cur_token + offset)].value;
}

Token Lexer::peek_token(uint offset) const {
  return tokens[circular_lu(cur_token + offset)];
}

bool Lexer::matches(TokenType type, uint offset) const {
  return this->peek_type(offset) == type;
}

Lexer::State Lexer::save() {
  return State {
    .reader_state = reader.save(),
    .tokens = tokens,
    .cur_token = cur_token,
  };
}

void Lexer::restore(const State& state) {
  reader.restore(state.reader_state);
  cur_token = state.cur_token;
  tokens = state.tokens;
}

static int try_merge(Token& t1, Token& t2, Token& t3) {
  // look at the last three token and see if we can merge them
  bool tri_merged = true;
  if (t1.type == TokenType::Dot && t2.type == TokenType::Dot && t3.type == TokenType::Dot)
    t1.type = TokenType::Ellipsis;
  else
    tri_merged = false;
  if (tri_merged) {
    t1.value = t1.value + t2.value + t3.value;
    t1.span.absorb(t2.span);
    t1.span.absorb(t3.span);

    // we merged the previous two token into the third last, so
    // we need to refresh both these tokens
    return -2;
  }

  // look at the last two tokens and see if we can merge them
  bool bin_merged = true;
  if (t2.type == TokenType::Less && t3.type == TokenType::Equals)
    t2.type = TokenType::LessEqual;
  else if (t2.type == TokenType::Greater && t3.type == TokenType::Equals)
    t2.type = TokenType::GreaterEqual;
  else if (t2.type == TokenType::Equals && t3.type == TokenType::Equals)
    t2.type = TokenType::DoubleEquals;
  else if (t2.type == TokenType::Exclamation && t3.type == TokenType::Equals)
    t2.type = TokenType::ExclamationEquals;
  else if (t2.type == TokenType::Plus && t3.type == TokenType::Equals)
    t2.type = TokenType::PlusEquals;
  else if (t2.type == TokenType::Minus && t3.type == TokenType::Equals)
    t2.type = TokenType::MinusEquals;
  else if (t2.type == TokenType::Asterisk && t3.type == TokenType::Equals)
    t2.type = TokenType::AsteriskEquals;
  else if (t2.type == TokenType::ForwardSlash && t3.type == TokenType::Equals)
    t2.type = TokenType::DivideEquals;
  else if (t2.type == TokenType::And && t3.type == TokenType::And)
    t2.type = TokenType::DoubleAnd;
  else if (t2.type == TokenType::Pipe && t3.type == TokenType::Pipe)
    t2.type = TokenType::DoublePipe;
  else if (t2.type == TokenType::Minus && t3.type == TokenType::Greater)
    t2.type = TokenType::Arrow;
  else if (t2.type == TokenType::Plus && t3.type == TokenType::Plus)
    t2.type = TokenType::DoublePlus;
  else if (t2.type == TokenType::Minus && t3.type == TokenType::Minus)
    t2.type = TokenType::DoubleMinus;
  else
    bin_merged = false;
  if (bin_merged) {
    t2.value = t2.value + t3.value;
    t2.span.absorb(t2.span);

    // we merged the current token into the previous, so
    // we need to refresh 
    return -1;
  }

  return 0;
}

// get tokens for 0 -> MAX_LOOKAHEAD - 1
void Lexer::refresh_front_token_buffer() {
  for (int i = 0; i < MAX_LOOKAHEAD; ++i) {
    get_primitive_token(tokens[circular_lu(i)]);

    // make sure we have enough gap to merge tokens in back buffer
    // @todo assert on offset lookups (i.e. which parts of buffer guaranteed)
    static_assert(MAX_LOOKAHEAD > 3);
    i += try_merge(
      tokens[circular_lu(i - 2)],
      tokens[circular_lu(i - 1)],
      tokens[circular_lu(i - 0)]
    );
  }
}

// get tokens for MAX_LOOKAHEAD -> MAX_LOOKAHEAD*2 - 1
void Lexer::refresh_back_token_buffer() {
  for (int i = MAX_LOOKAHEAD; i < NUM_TOKENS; ++i) {
    get_primitive_token(tokens[circular_lu(i)]);

    // make sure we have enough gap to merge tokens in front buffer
    static_assert(MAX_LOOKAHEAD > 3);
    i += try_merge(
      tokens[circular_lu(i - 2)],
      tokens[circular_lu(i - 1)],
      tokens[circular_lu(i - 0)]
    );
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

    //"[^"]*|\\""
    if (reader.peek_char() == '"') {
      int len = 0;

      do {
        buf[len++] = reader.consume_char();
      } while (len < MAX_TOKEN_LEN &&
        !(len >= 2 && buf[len-2] != '\\' && buf[len-1] == '"') && // close quote which is not escaped
        reader.peek_char() != '\n' && !reader.eof() && !reader.error() // end of line or file
      );

      t.span.end = reader.get_consumed_location();
      t.type = TokenType::String,
      t.value = std::string(buf, len);
      return; 
    }

    // [_a-zA-Z][a-zA-Z_0-9]
    if (isalpha(reader.peek_char()) || reader.peek_char() == '_') {
      int len = 0;

      do {
        buf[len++] = reader.consume_char();
      } while (len < MAX_TOKEN_LEN &&
        (isalnum(reader.peek_char()) || reader.peek_char() == '_')
      );

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
      else if (t.value == "struct")
        t.type = TokenType::Struct;
      else if (t.value == "enum")
        t.type = TokenType::Enum;
      else if (t.value == "union")
        t.type = TokenType::Union;
      else if (t.value == "undefined")
        t.type = TokenType::Undefined;
      else if (t.value == "void")
        t.type = TokenType::Void;
      else
        t.type = TokenType::Name;
      return;
    }

    // [0-9]*(\.[0-9]*)?f?
    if (isdigit(reader.peek_char()) || reader.peek_char() == '.') {
      bool is_float = reader.peek_char() == '.';

      int len = 0;
      // (\.)?[0-9]*
      do {
        buf[len++] = reader.consume_char();
      } while (len < MAX_TOKEN_LEN && isdigit(reader.peek_char()));

      if (reader.peek_char() == 'f') {
        is_float = true;

        reader.consume_char();
      }
      else if (!is_float && reader.peek_char() == '.') {
        is_float = true;

        // \.[0-9]*
        do {
          buf[len++] = reader.consume_char();
        } while (len < MAX_TOKEN_LEN && isdigit(reader.peek_char()));

        // consume extra 'f' if it is provided
        if (reader.peek_char() == 'f')
          reader.consume_char();
      }
      // lone '.' character, this is not a float / integer
      else if (is_float && len == 1) {
        t.span.end = reader.get_consumed_location();
        t.type = TokenType::Dot;
        t.value = std::string(1, '.');
        return;
      }

      t.span.end = reader.get_consumed_location();
      t.type = is_float ? TokenType::Float : TokenType::Integer,
      t.value = std::string(buf, len);
      return;
    }

    // unmatched character
    {
      char c = reader.consume_char();

      if (reader.eof()) {
        t.span.end = t.span.start;
        t.type = TokenType::End;
        return;
      }
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
    case TokenType::Unknown          : return "Unknown";            
    case TokenType::Name             : return "Name";
    case TokenType::Return           : return "Return";
    case TokenType::Extern           : return "Extern";
    case TokenType::Integer          : return "Integer";
    case TokenType::Float            : return "Float";
    case TokenType::String           : return "String";
    case TokenType::If               : return "If";
    case TokenType::Else             : return "Else";
    case TokenType::While            : return "While";
    case TokenType::Struct           : return "Struct";
    case TokenType::Enum             : return "Enum";
    case TokenType::Union            : return "Union";
    case TokenType::Undefined        : return "Undefined";
    case TokenType::Void             : return "Void";
    case TokenType::LessEqual        : return "LessEqual";
    case TokenType::GreaterEqual     : return "GreaterEqual";
    case TokenType::DoubleEquals     : return "DoubleEquals";
    case TokenType::ExclamationEquals: return "ExclamationEquals";
    case TokenType::PlusEquals       : return "PlusEquals";
    case TokenType::MinusEquals      : return "MinusEquals";
    case TokenType::AsteriskEquals   : return "AsteriskEquals";
    case TokenType::DivideEquals     : return "DivideEquals";
    case TokenType::DoubleAnd        : return "DoubleAnd";
    case TokenType::DoublePipe       : return "DoublePipe";
    case TokenType::DoublePlus       : return "DoublePlus";
    case TokenType::DoubleMinus      : return "DoubleMinus";
    case TokenType::Arrow            : return "Arrow";
    case TokenType::Ellipsis         : return "Ellipsis";
    default:
      return "'" + std::string(1, static_cast<TokenTypeUnderlying>(v)) + "'";
  }
}

std::string to_string(const Token& v) {
  return  "'" + v.value + "' (" + to_string(v.type) + ")";
}
