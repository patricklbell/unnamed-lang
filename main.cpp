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
#include "ast.hpp"
#include "code_generation.hpp"
#include "type_generation.hpp"

static std::string_view strip_file_extension(std::string_view in) {
  size_t lastindex = in.find_last_of("."); 
  return in.substr(0, lastindex); 
}

static std::string usage_message(int argc, char** argv) {
  if (argc > 0)
    return "\"" + std::string(argv[0]) + " --help\" for help";
  return "\"langc --help\" for help";
}

static std::string help_message() {
  return "--help      : Print this help message\n"
         "--ast       : Print the ast after parsing\n"
         "--no-emit   : Do not emit object code\n"
         "--jit       : Run modules just-in-time in the order provided\n";
}

struct ConsoleOptions {
  bool print_ast  = false;
  bool no_emit    = false;
  bool jit        = false;
};

int main(int argc, char** argv) {
  if (argc <= 1) {
    std::cerr << "No input files, exiting " + usage_message(argc, argv) + ".\n";
    return 1;
  }
  
  ConsoleOptions options;
  for (int i = 1; i < argc; ++i) {
    auto arg = std::string(argv[i]);
    if (arg.rfind("--", 0) != 0)
      continue;

    if (arg == "--help")
      std::cout << help_message();
    if (arg == "--ast")
      options.print_ast = true;
    if (arg == "--no-emit")
      options.no_emit = true;
    if (arg == "--jit")
      options.jit = true;
  }

  // logging and tracking for files
  FileManager fm;
  ConsoleLogger logger;
  
  // parse all source file provided
  // @todo build ast as needed
  AST ast;
  std::vector<std::string> module_names;
  for (int i = 1; i < argc; ++i) {
    auto path = std::string(argv[i]);
    if (path.rfind("--", 0) == 0)
      continue;
      
    auto& module_name = module_names.emplace_back(strip_file_extension(path));

    Reader source(path);
    Lexer lexer(source, fm.add(path));

    parse_module_ast(ast, module_name, lexer, logger);
  }

  if (options.print_ast)
    print_ast(ast);

  // early exit if parsing failed
  // @todo syntax errors which still permit type generation
  if (logger.is_error()) {
    logger.commit(fm);
    return 1;
  }

  TypeInfo type_info;
  typegen(ast, type_info, logger);

  // print warnings etc.
  bool failed = logger.is_error();
  logger.commit(fm);
  if (failed)
    return 1;

  CompilerContext context;
  codegen(ast, context, type_info);

  if (options.jit) {
    for (auto &module_name : module_names) {
      jit_run_module(context, module_name);
    }
  }

  // compile modules to object files
  if (!options.no_emit)
    return emit_object_code(context);
}