#include <iostream>

extern "C" {
    int64_t fibonacci(int64_t);
}

int main() {
  std::cout << "F(10) = " << fibonacci(10) << std::endl;
}
