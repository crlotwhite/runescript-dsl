# Test configuration for RuneScript DSL

# Find Google Test
find_package(GTest QUIET)
if(NOT GTest_FOUND)
    # Download and build Google Test if not found
    include(FetchContent)
    FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9fd7906fc5eb2be09723bb1.zip
    )
    
    # For Windows: Prevent overriding the parent project's compiler/linker settings
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
endif()

# Include test directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/src)

# Test source files
set(TEST_SOURCES
    test_parser.cpp
    test_interpreter.cpp
    test_ast.cpp
    test_examples.cpp
    test_main.cpp
)

# Create test executable
add_executable(runescript_tests ${TEST_SOURCES})

# Link libraries
target_link_libraries(runescript_tests
    runescript_core
    gtest
    gtest_main
    pthread
)

# Enable testing
enable_testing()

# Add test cases
add_test(NAME RuneScriptTests COMMAND runescript_tests)

# Set test properties
set_tests_properties(RuneScriptTests PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    TIMEOUT 30
)

# Copy test data files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/simple.rcs ${CMAKE_CURRENT_BINARY_DIR}/data/simple.rcs COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/korean_test.spell ${CMAKE_CURRENT_BINARY_DIR}/data/korean_test.spell COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/test.rune ${CMAKE_CURRENT_BINARY_DIR}/data/test.rune COPYONLY)

# Additional test targets
add_custom_target(test_verbose
    COMMAND ${CMAKE_CTEST_COMMAND} --verbose
    DEPENDS runescript_tests
)

add_custom_target(test_coverage
    COMMAND ${CMAKE_CTEST_COMMAND} --verbose
    COMMAND gcov ${TEST_SOURCES}
    DEPENDS runescript_tests
)
