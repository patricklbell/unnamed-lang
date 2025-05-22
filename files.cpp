#include "files.hpp"
#include "logging.hpp"

void TextSpan::absorb(const TextSpan& span) {
  LANG_ASSERT(src_id == span.src_id, "Tried to merge spans from different source files");

  if (span.is_null())
    return;
  if (is_null()) {
    start = span.start;
    end = span.end;
    return;
  }

  LANG_ASSERT(start.line <= end.line, "Malformed row span");
  LANG_ASSERT(start.line != end.line || start.column <= end.column, "Malformed column span");
  LANG_ASSERT(span.start.line <= span.end.line, "Malformed row span");
  LANG_ASSERT(span.start.line != span.end.line || span.start.column <= span.end.column, "Malformed column span");

  if (start.line == span.start.line) {
    start.column = std::min(start.column, span.start.column);
  }
  if (end.line == span.end.line) {
    end.column = std::max(end.column, span.end.column);
  }

  if (span.start.line < start.line) {
    start.line = span.start.line;
    start.column = span.start.column;
  }
  if (span.end.line > end.line) {
    end.line = span.end.line;
    end.column = span.end.column;
  }
}

bool TextSpan::is_null() const {
  return start.line == -1 || start.column == -1 || start.pos == -1;
}

std::string to_string(const TextLocation& v) {
  return std::to_string(v.line) + ":" + std::to_string(v.column);
}

std::string to_string(const TextSpan& v) {
  return to_string(v.start) + "-" + to_string(v.end);
}

Reader::Reader(const std::string& filename) {
  fd = fopen(filename.c_str(), "r");
  if(fd != nullptr) {
    consume_char();
  }
}

Reader::~Reader() {
  if (fd != nullptr)
    fclose(fd);
  fd = nullptr;
}

bool Reader::error() {
  return fd == nullptr;
}

bool Reader::eof() {
  return fd == nullptr || lc == EOF;
}

char Reader::consume_char() {
  clin = lin;
  ccol = col;

  pos++;
  col++;
  if (lc == '\n') {
    lin++;
    col = 1;
  }

  char c = lc;
  lc = getc(fd);
  return c;
}

char Reader::peek_char() {
  return lc;
}

TextLocation Reader::get_consumed_location() {
  return TextLocation{ .line = clin, .column = ccol, .pos = pos - 1 };
}

TextLocation Reader::get_peek_location() {
  return TextLocation{ .line = lin, .column = col, .pos = pos };
}

int FileManager::add(std::string path) {
  id_to_path[++counter] = path;
  return counter;
}

std::string FileManager::get_path(int id) const {
  auto res = id_to_path.find(id);
  return res != id_to_path.end() ? res->second : "";
}

