langc: *.cpp
	clang++ -g -O0 -Xlinker --export-dynamic *.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -o langc