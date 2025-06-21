#include <gtest/gtest.h>

// Main test runner
int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    
    // Set up test environment
    std::cout << "Running RuneScript DSL Tests..." << std::endl;
    std::cout << "================================" << std::endl;
    
    int result = RUN_ALL_TESTS();
    
    if (result == 0) {
        std::cout << "\nAll tests passed successfully!" << std::endl;
    } else {
        std::cout << "\nSome tests failed. Please check the output above." << std::endl;
    }
    
    return result;
}
