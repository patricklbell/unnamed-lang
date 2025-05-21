#include "code_generation.hpp"

#include <iostream>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putcharf - putchar that takes a float and returns 0.
extern "C" DLLEXPORT float putcharf(float X) {
  fputc((char)X, stderr);
  return 0;
}

void jit_run_module(CompilerContext& ctx, std::string& name) {
    auto module_lu = ctx.modules.find(name);
    if (module_lu == ctx.modules.end()) {
      std::cerr << "No module \"" << name << "\" found\n";
      return;
    }
  
    auto& module = module_lu->second;
  
    module.llvm_module->setDataLayout(ctx.llvm_jit->getDataLayout());
  
    // Create a ResourceTracker to track JIT'd memory allocated to our
    // anonymous expression -- that way we can free it after executing.
    auto RT = ctx.llvm_jit->getMainJITDylib().createResourceTracker();
  
    auto TSM = llvm::orc::ThreadSafeModule(
      std::move(module.llvm_module),
      std::move(ctx.llvm_context)
    );
    ctx.modules.erase(ctx.modules.find(name));
  
    if (auto e = ctx.llvm_jit->addModule(std::move(TSM), RT)) {
      std::cerr << llvm::toString(std::move(e)) << "\n";
      return;
    }
  
    // Search the JIT for the __anon_expr symbol.
    auto ExprSymbol = ctx.llvm_jit->lookup("main");
    if (auto e = ExprSymbol.takeError()) {
      std::cerr << llvm::toString(std::move(e)) << "\n";
      return;
    }
  
    // Get the symbol's address and cast it to the right type (takes no
    // arguments, returns a float) so we can call it as a native function.
    float (*FP)() = ExprSymbol->getAddress().toPtr<float (*)()>();
    std::cout << "Evaluated to " << FP() << "\n";
  
    // Delete the anonymous expression module from the JIT.
    if (auto e = RT->remove()) {
      std::cerr << llvm::toString(std::move(e)) << "\n";
    }
  }
  