#pragma once

#include <string>
#include <vector>
#include "lexer.hpp"

enum class Errors : int64_t {
  Syntax = (1 << 0),
  Type 	 = (1 << 1),
};

class Logger {
public:
  virtual ~Logger() = default;
  virtual void log(Errors error, const std::string& message, TextSpan span) = 0;
};

class ConsoleLogger : public Logger {
public:
  virtual void log(Errors error, const std::string& message, TextSpan span) override;

  bool is_error();

  void commit(const FileManager& file_manager); 

private:
  struct LogMessage {
    Errors error; 
    std::string message;
    TextSpan span;
  };

  std::vector<LogMessage> messages;
};

#define LANG_DEBUG 1

/// Trace function, needs to be overridden by application. This should output a line of text to the log / TTY.
using TraceFunction = void (*)(const char *inFMT, ...);
extern TraceFunction trace;

// Always turn on asserts in debug mode
#if defined(LANG_DEBUG)
	#define LANG_ENABLE_ASSERTS
#endif

#ifdef LANG_ENABLE_ASSERTS
	/// Function called when an assertion fails. This function should return true if a breakpoint needs to be triggered
	using AssertFailedFunction = bool(*)(const char *inExpression, const char *inMessage, const char *inFile, int inLine);
	extern AssertFailedFunction assert_failed;

	// Helper functions to pass message on to failed function
	struct AssertLastParam { };
	inline bool AssertFailedParamHelper(const char *inExpression, const char *inFile, int inLine, AssertLastParam) { return assert_failed(inExpression, nullptr, inFile, inLine); }
	inline bool AssertFailedParamHelper(const char *inExpression, const char *inFile, int inLine, const char *inMessage, AssertLastParam) { return assert_failed(inExpression, inMessage, inFile, inLine); }

	/// Main assert macro, usage: LANG_ASSERT(condition, message) or LANG_ASSERT(condition)
	#define LANG_ASSERT(inExpression, ...)	do { !(inExpression) && AssertFailedParamHelper(#inExpression, __FILE__, int(__LINE__), ##__VA_ARGS__, AssertLastParam()); } while (false)

	#define LANG_IF_ENABLE_ASSERTS(...)		__VA_ARGS__
#else
	#define LANG_ASSERT(...)					((void)0)

	#define LANG_IF_ENABLE_ASSERTS(...)
#endif // LANG_ENABLE_ASSERTS
