#include "code_generation.hpp"

#include <iostream>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int putchar(int X) {
  fputc((char)X, stderr);
  return 0;
}

void jit_run_and_delete_modules(CompilerContext& ctx) {  
  // create a ResourceTracker to track JIT'd memory allocated
  auto resource_tracker = ctx.llvm_jit->getMainJITDylib().createResourceTracker();

  bool error_adding_module = false;
  for (auto& mm : ctx.modules) {
    auto& module = mm.second;

    module.llvm_module->setDataLayout(ctx.llvm_jit->getDataLayout());

    auto orc_module = llvm::orc::ThreadSafeModule(
      std::move(module.llvm_module),
      std::move(ctx.llvm_context)
    );

    if (auto e = ctx.llvm_jit->addModule(std::move(orc_module), resource_tracker)) {
      std::cerr << llvm::toString(std::move(e)) << "\n";
      error_adding_module = true;
    }
  }
  // make sure unique pointers aren't reused
  ctx.modules.clear();

  if (!error_adding_module) {
    // search the JIT for the main symbol.
    auto main_symbol = ctx.llvm_jit->lookup("main");
    if (auto e = main_symbol.takeError()) {
      std::cerr << llvm::toString(std::move(e)) << "\n";
    } else {
      // get main symbol's address and cast it to the right type so we can call it as a native function.
      int (*main_fptr)() = main_symbol->getAddress().toPtr<int (*)()>();
      int exit_code = main_fptr();
    
      std::cout << "JIT exited with code " << exit_code << "\n";
    }
  }

  // clear resources tracked by JIT
  if (auto e = resource_tracker->remove()) {
    std::cerr << llvm::toString(std::move(e)) << "\n";
  }
}
  