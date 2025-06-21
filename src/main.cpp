#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include "core/parser.hpp"
#include "core/interpreter.hpp"

void printUsage(const char* program_name) {
    std::cout << "Usage: " << program_name << " [options] <script.rcs>\n";
    std::cout << "\nOptions:\n";
    std::cout << "  --help, -h          Show this help message\n";
    std::cout << "  --version, -v       Show version information\n";
    std::cout << "  --caster <file>     Use caster configuration file\n";
    std::cout << "  --spell <module>    Load spell module\n";
    std::cout << "  --rune <module>     Load rune module\n";
    std::cout << "  --debug             Enable debug mode\n";
    std::cout << "\nExamples:\n";
    std::cout << "  " << program_name << " hello.rcs\n";
    std::cout << "  " << program_name << " --caster pipeline.caster script.rcs\n";
    std::cout << "  " << program_name << " --spell korean_g2p.spell --rune korean.rune text.rcs\n";
}

void printVersion() {
    std::cout << "RuneScript DSL Interpreter v0.1.0\n";
    std::cout << "Built with C++20\n";
    std::cout << "Copyright (c) 2025 RuneScript DSL Project\n";
}

std::string readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main(int argc, char* argv[]) {
    try {
        // Parse command line arguments
        std::vector<std::string> args(argv, argv + argc);
        std::string script_file;
        std::string caster_file;
        std::vector<std::string> spell_modules;
        std::vector<std::string> rune_modules;
        bool debug_mode = false;
        
        for (size_t i = 1; i < args.size(); ++i) {
            const std::string& arg = args[i];
            
            if (arg == "--help" || arg == "-h") {
                printUsage(argv[0]);
                return 0;
            } else if (arg == "--version" || arg == "-v") {
                printVersion();
                return 0;
            } else if (arg == "--debug") {
                debug_mode = true;
            } else if (arg == "--caster" && i + 1 < args.size()) {
                caster_file = args[++i];
            } else if (arg == "--spell" && i + 1 < args.size()) {
                spell_modules.push_back(args[++i]);
            } else if (arg == "--rune" && i + 1 < args.size()) {
                rune_modules.push_back(args[++i]);
            } else if (arg.front() != '-') {
                script_file = arg;
            } else {
                std::cerr << "Unknown option: " << arg << std::endl;
                printUsage(argv[0]);
                return 1;
            }
        }
        
        if (script_file.empty()) {
            std::cerr << "Error: No script file specified" << std::endl;
            printUsage(argv[0]);
            return 1;
        }
        
        if (debug_mode) {
            std::cout << "Debug mode enabled" << std::endl;
            std::cout << "Script file: " << script_file << std::endl;
            if (!caster_file.empty()) {
                std::cout << "Caster file: " << caster_file << std::endl;
            }
        }
        
        // Initialize interpreter
        runescript::core::Interpreter interpreter;
        
        // Load spell modules
        for (const auto& module : spell_modules) {
            if (debug_mode) {
                std::cout << "Loading spell module: " << module << std::endl;
            }
            try {
                interpreter.loadSpellModule(module);
            } catch (const std::exception& e) {
                std::cerr << "Warning: Failed to load spell module " << module 
                         << ": " << e.what() << std::endl;
            }
        }
        
        // Load rune modules
        for (const auto& module : rune_modules) {
            if (debug_mode) {
                std::cout << "Loading rune module: " << module << std::endl;
            }
            try {
                interpreter.loadRuneModule(module);
            } catch (const std::exception& e) {
                std::cerr << "Warning: Failed to load rune module " << module 
                         << ": " << e.what() << std::endl;
            }
        }
        
        // Load and parse caster configuration if specified
        if (!caster_file.empty()) {
            try {
                std::string caster_content = readFile(caster_file);
                runescript::core::Parser caster_parser(caster_content);
                auto caster_program = caster_parser.parseCaster();
                
                if (debug_mode) {
                    std::cout << "Loaded caster configuration with:\n";
                    std::cout << "  Load directives: " << caster_program->load_directives.size() << std::endl;
                    std::cout << "  Run directives: " << caster_program->run_directives.size() << std::endl;
                    std::cout << "  Backend: " << caster_program->backend << std::endl;
                }
                
                // Load modules specified in caster file
                for (const auto& module : caster_program->load_directives) {
                    try {
                        if (module.ends_with(".spell")) {
                            interpreter.loadSpellModule(module);
                        } else if (module.ends_with(".rune")) {
                            interpreter.loadRuneModule(module);
                        }
                    } catch (const std::exception& e) {
                        std::cerr << "Warning: Failed to load module " << module 
                                 << " from caster: " << e.what() << std::endl;
                    }
                }
            } catch (const std::exception& e) {
                std::cerr << "Warning: Failed to load caster file: " << e.what() << std::endl;
            }
        }
        
        // Read and parse the main script
        std::string script_content = readFile(script_file);
        
        if (debug_mode) {
            std::cout << "\nParsing script..." << std::endl;
        }
        
        runescript::core::Parser parser(script_content);
        auto program = parser.parseRCS();
        
        if (!program) {
            std::cerr << "Error: Failed to parse script" << std::endl;
            return 1;
        }
        
        if (debug_mode) {
            std::cout << "Parsed " << program->statements.size() << " statements" << std::endl;
            std::cout << "\nExecuting script..." << std::endl;
        }
        
        // Execute the program
        auto result = interpreter.execute(*program);
        
        if (debug_mode) {
            std::cout << "\nExecution completed successfully" << std::endl;
            
            // Print final result if not empty
            std::string result_str = interpreter.valueToString(result);
            if (!result_str.empty() && result_str != "") {
                std::cout << "Final result: " << result_str << std::endl;
            }
        }
        
    } catch (const runescript::core::RuntimeError& e) {
        std::cerr << "Runtime Error: " << e.what() << std::endl;
        return 1;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Unknown error occurred" << std::endl;
        return 1;
    }
    
    return 0;
}
