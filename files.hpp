#pragma once

#include <string>
#include <unordered_map>

class FileManager {
public:
  int add(std::string path);
  std::string get_path(int id) const;

private:
  int counter = 0;
  std::unordered_map<int, std::string> id_to_path;
};

struct TextLocation {
  int line = -1;
  int column = -1;
  int pos = -1;
};

struct TextSpan {
  TextLocation start;
  TextLocation end;
  int src_id;
  
  void absorb(const TextSpan& span);
  bool is_null() const;
};

std::string to_string(const TextLocation& v);
std::string to_string(const TextSpan& v);

class Reader {
public:
  Reader() = delete;
  Reader (const Reader&) = delete;
  Reader& operator= (const Reader&) = delete;
  ~Reader();
  Reader(const std::string& path);

  bool eof();
  bool error();
  char consume_char();
  char peek_char();

  TextLocation get_peek_location();
  TextLocation get_consumed_location();

  struct State {
    int64_t file_position;
    int clin;
    int ccol;
    int lin;
    int col;
    int pos;
    char lc;
  };
  State save();
  void restore(const State& state);

private:
  int clin = 0;
  int ccol = 0;

  int lin = 1;
  int col = 0;
  int pos = -1;
  char lc = 0;

  FILE* fd;
};

