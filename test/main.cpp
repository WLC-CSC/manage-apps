#include <iostream>
#include <chrono>
#include <thread>

int main() {
    while (true) {
        std::cout << "Hello, World!" << std::endl;
        throw std::runtime_error("bad");
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }
}
