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

static std::string_view strip_file_extension(std::string_view in) {
  size_t lastindex = in.find_last_of("."); 
  return in.substr(0, lastindex); 
}

int main(int argc, char** argv) {
  if (argc <= 1) {
    std::cerr << "No input files, exiting.\n";
    return 1;
  }

  FileManager fm;
  ConsoleLogger logger;

  CompilerContext context;

  for (int i = 1; i < argc; ++i) {
    auto path = std::string(argv[i]);
    std::string module_name(strip_file_extension(path));

    Reader source(path);
    Lexer lexer(source, fm.add(path));

    AST ast;
    make_ast(ast, lexer, logger);
    print_ast(ast);

    if (logger.is_error()) {
      logger.commit(fm);
      return 1;
    }

    codegen_module(ast, context, module_name);
  }

  // print warnings etc.
  logger.commit(fm);

  // compile modules to object files
  return emit_object_code(context);
}
