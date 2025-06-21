cmake_minimum_required(VERSION 3.16)
project(RuneScriptDSL VERSION 0.1.0 LANGUAGES CXX)

# Set C++20 standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Find required packages
find_package(PkgConfig REQUIRED)

# Compiler-specific options
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic")
    set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
    set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
endif()

# Include directories
include_directories(src)

# Source files
set(CORE_SOURCES
    src/core/parser.cpp
    src/core/ast.cpp
    src/core/interpreter.cpp
)

set(HEADER_FILES
    src/core/parser.hpp
    src/core/ast.hpp
    src/core/interpreter.hpp
)

# Create core library
add_library(runescript_core ${CORE_SOURCES})
target_include_directories(runescript_core PUBLIC src)

# Main executable
add_executable(runescript src/main.cpp)
target_link_libraries(runescript runescript_core)

# Tests (optional, when available)
option(BUILD_TESTS "Build test suite" ON)
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Install rules
install(TARGETS runescript DESTINATION bin)
install(TARGETS runescript_core DESTINATION lib)
install(FILES ${HEADER_FILES} DESTINATION include/runescript)

# Examples
install(DIRECTORY examples/ DESTINATION share/runescript/examples
        FILES_MATCHING PATTERN "*.rcs" PATTERN "*.spell" PATTERN "*.rune" PATTERN "*.caster")
