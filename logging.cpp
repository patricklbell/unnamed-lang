#include "logging.hpp"
#include <iomanip>
#include <ios>
#include <string>
#include <unordered_map>
#include <fstream>
#include <iostream>
#include <cctype>
#include <cstring>

static std::string to_string(Errors error) {
  switch (error) {
    case Errors::Syntax:        return "syntax";
    default:                    return "unkown";
  }
}

void ConsoleLogger::log(Errors error, const std::string& message, TextSpan span) {
  LANG_ASSERT(span.start.pos <= span.end.pos);
  LANG_ASSERT(span.start.line <= span.end.line);
  LANG_ASSERT(span.start.column <= span.end.column);

  messages.emplace_back(
    LogMessage{
      .error = error,
      .message = message,
      .span = span,
    }
  );
}

void ConsoleLogger::commit(const FileManager& file_manager) {
  std::unordered_map<int, std::vector<LogMessage*>> file_to_messages;
  for (auto& m : messages) {
    if (file_to_messages.find(m.span.src_id) == file_to_messages.end())
      file_to_messages[m.span.src_id] = std::vector<LogMessage*>{};

    file_to_messages[m.span.src_id].push_back(&m);
  }

  std::string line_buf;
  for (auto& [src_id, msgs] : file_to_messages) {
    auto path = file_manager.get_path(src_id);
    std::ifstream file;

    for (auto m_ptr : msgs) {
      auto& span = m_ptr->span;
      auto& error = m_ptr->error;
      auto& message = m_ptr->message;

      if (path != "")
        std::cout << path << ":" << to_string(span.start) << ": ";
      std::cout << to_string(error) << ": " << message << "\n";
      if (path == "" || span.start.pos < 0)
        continue;

      if (file.bad())
        file.close();
      else if (file.eof() || file.fail())
        file.clear();
      if (!file.is_open() || file.bad())
        file.open(path);

      if (file) {
        file.seekg(span.start.pos);
        int line = span.start.line;

        // go to beginning of line
        while(file.tellg() != 0 && file.unget() && file.peek() != '\n') {}
        if (file.peek() == '\n') {
          // go back an extra line if possible
          while(file.tellg() != 0 && file.unget()) {
            line = span.start.line - 1;
            if (file.peek() == '\n')
              break;
          }

          // consume \n from previous line
          file.get();
        }

        int left_pad = std::to_string(span.end.line + 1).length() + 1;
        for (; line <= span.end.line + 1; ++line) {
          std::getline(file, line_buf);
          std::cout << std::setfill(' ') << std::setw(left_pad) << std::right << line;
          std::cout << " | " << line_buf << "\n";

          if (line >= span.start.line && line <= span.end.line) {
            std::cout << std::string(left_pad + 3, ' ');

            for (int col = 1; col <= line_buf.length(); ++col) {
              char c = line_buf[col - 1];

              if (isspace(c))
                std::cout << c;
              else if (line == span.start.line && col < span.start.column)
                std::cout << ' ';
              else if (line == span.end.line && col > span.end.column)
                std::cout << ' ';
              else
                std::cout << '^';
            }

            std::cout << "\n";
          }
        }
      }
    }
  }

  messages.clear();
}


static void dummy_trace([[maybe_unused]] const char *inFMT, ...) {
  LANG_ASSERT(false);
};

TraceFunction trace = dummy_trace;

#ifdef LANG_ENABLE_ASSERTS

static bool dummy_assert_failed(const char *inExpression, const char *inMessage, const char *inFile, int inLine) {
  std::cerr << "Assert failed \"" << inExpression << "\" at " << inFile << ":" << inLine << " " << inMessage << std::endl;
  return true; // Trigger breakpoint
};

AssertFailedFunction assert_failed = dummy_assert_failed;

#endif // LANG_ENABLE_ASSERTS
