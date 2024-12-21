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

    Reader source(path);
    Lexer lexer(source, fm.add(path));

    AST ast;
    make_ast(ast, lexer, logger);
    print_ast(ast);

    codegen_module(ast, context, path);

    // Print out all of the generated code.
    context.modules[path].llvm_module->print(llvm::errs(), nullptr);

    // run the module
    run_module(context, path);
  }

  logger.commit(fm);

  return 0;
}
