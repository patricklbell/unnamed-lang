#include <cstdio>
#include <iostream>
#include <llvm/Support/TargetSelect.h>
#include <memory>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cctype>

#include <llvm/Support/raw_ostream.h>

#include "logging.hpp"
#include "lexer.hpp"
#include "parse.hpp"
#include "code_generation.hpp"
#include "type_generation.hpp"

static int link_object_files_to_executable(const CompilerContext& ctx, const std::string& out) {
  std::string cmd = "cc";
  for (auto& mm : ctx.modules) {
    cmd += " " + mm.first + ".o";
  }
  cmd += " -o " + out;
  return system(cmd.c_str());
}

static std::string_view strip_file_extension(std::string_view in) {
  size_t lastindex = in.find_last_of("."); 
  return in.substr(0, lastindex); 
}

static std::string usage_message(int argc, char** argv) {
  return "run \"" + (argc > 0 ? std::string(argv[0]) : "langc") + " --help\" for help";
}

static std::string help_message(int argc, char** argv) {
  return "Usage: " + (argc > 0 ? std::string(argv[0]) : "langc") + " [options] file...\n"
         "Options:\n"
         "  --help        Print this help message.\n"
         "  --ast         Print the ast after parsing.\n"
         "  --no-emit     Do not emit object code.\n"
         "  --jit         Run modules just-in-time in the order provided.\n"
         "  -o <file>     Output executable into <file>.\n";
}

struct ConsoleOptions {
  bool print_ast = false;
  bool no_emit   = false;
  bool jit       = false;

  std::vector<std::string> source_files;
  std::string emit_executable_path = "a.out";
};

static bool parse_command_line_options(int argc, char** argv, ConsoleOptions& options) {
  bool error_occurred = false;
  bool print_help = false;
  for (int i = 1; i < argc; ++i) {
    auto arg = std::string(argv[i]);
    if (arg.rfind("-", 0) != 0) {
      options.source_files.emplace_back(arg);
      continue;
    }

    if (arg == "--help") {
      print_help = true;
    } else if (arg == "--ast") {
      options.print_ast = true;
    } else if (arg == "--no-emit") {
      options.no_emit = true;
    } else if (arg == "--jit") {
      options.jit = true;
    } else if (arg == "-o") {
      if (i + 1 < argc) {
        std::cerr << "Expected output name after -o, " << usage_message(argc, argv) << ".\n";
        error_occurred = true;
      } else {
        i++;
        options.emit_executable_path = std::string(argv[i]);
      }
    } else {
      std::cerr << "Unexpected argument \"" + arg + "\", " << usage_message(argc, argv) << ".\n";
      error_occurred = true;
    }
  }

  if (print_help) {
    std::cout << help_message(argc, argv);
    exit(0);
  }

  return !error_occurred;
}

int main(int argc, char** argv) {  
  ConsoleOptions opts;
  if (!parse_command_line_options(argc, argv, opts))
    return 1;
  
  if (opts.source_files.empty()) {
    std::cerr << "No source files, " << usage_message(argc, argv) << ".\n";
    return 0;
  }

  // logging and tracking for files
  FileManager fm;
  ConsoleLogger logger;
  
  // parse each source file
  // @todo build ast as needed
  bool failed_to_read = false;
  AST ast;
  for (const auto& path : opts.source_files) {
    Reader source(path);
    if (source.error()) {
      std::cerr << "Failed to read \"" << path << "\".\n";
      failed_to_read = true;
    }
    // don't bother processing if any files are missing
    if (failed_to_read) {
      continue;
    }

    Lexer lexer(source, fm.add(path));
    parse_module_ast(ast, std::string(strip_file_extension(path)), lexer, logger);
  }
  if (failed_to_read) {
    std::cerr << "One or more files could not be read, exiting.\n";
    return 1;
  }

  if (opts.print_ast)
    print_ast(ast);

  // early exit if parsing failed
  // @todo syntax errors which still permit type generation
  if (logger.is_error()) {
    logger.commit(fm);
    return 1;
  }

  // // generate and check type information
  // TypeInfo type_info;
  // typegen(ast, type_info, logger);

  // bool failed = logger.is_error();
  // logger.commit(fm);
  // if (failed)
  //   return 1;

  // // create llvm code
  // CompilerContext context;
  // codegen(ast, context, type_info);

  // // compile modules to object files and executable
  // if (!opts.no_emit) {
  //   emit_object_code(context) || 
  //   link_object_files_to_executable(context, opts.emit_executable_path);
  // }

  // // @note deletes modules
  // if (opts.jit) {
  //   jit_run_and_delete_modules(context);
  // }
}