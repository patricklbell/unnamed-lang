#include <iostream>

extern "C" {
    float fibonacci(float);
}

int main() {
  std::cout << "F(10) = " << fibonacci(10) << std::endl;
}
