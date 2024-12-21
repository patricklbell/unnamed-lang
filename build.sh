clang++ -g -O3 -Xlinker --export-dynamic *.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -o unnamedc
