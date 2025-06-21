#pragma once

#include "ast.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace runescript {
namespace core {

// Value type for runtime values
using Value = std::variant<std::string, double, std::vector<std::string>, bool>;

// Runtime environment for variables and functions
class Environment {
public:
    Environment(Environment* parent = nullptr);
    
    void define(const std::string& name, const Value& value);
    Value get(const std::string& name) const;
    void set(const std::string& name, const Value& value);
    
private:
    std::unordered_map<std::string, Value> variables_;
    Environment* parent_;
};

// Function signature for built-in functions
using BuiltinFunction = std::function<Value(const std::vector<Value>&)>;

// Main interpreter class
class Interpreter {
public:
    Interpreter();
    
    // Module loading
    void loadSpellModule(const std::string& filename);
    void loadRuneModule(const std::string& filename);
    
    // Execution
    Value execute(const ast::Program& program);
    Value evaluateExpression(const ast::Expr& expr);
    
    // Environment management
    void setVariable(const std::string& name, const Value& value);
    Value getVariable(const std::string& name) const;
    
    // Built-in function registration
    void registerBuiltin(const std::string& name, BuiltinFunction func);
    
private:
    std::unique_ptr<Environment> global_env_;
    Environment* current_env_;
    std::unordered_map<std::string, BuiltinFunction> builtins_;
    
    // Built-in function implementations
    void initializeBuiltins();
    
    // Helper methods
    Value callFunction(const std::string& name, const std::vector<Value>& args);
    Value evaluateAssignment(const ast::Assignment& assignment);
    Value evaluateCall(const ast::CallExpr& call);
    Value evaluatePipe(const ast::PipeExpr& pipe);
    
    // Type conversion helpers
    std::string valueToString(const Value& value) const;
    double valueToNumber(const Value& value) const;
    bool valueToBool(const Value& value) const;
};

// Exception types
class RuntimeError : public std::runtime_error {
public:
    explicit RuntimeError(const std::string& message) 
        : std::runtime_error(message) {}
};

class UndefinedVariableError : public RuntimeError {
public:
    explicit UndefinedVariableError(const std::string& name)
        : RuntimeError("Undefined variable: " + name) {}
};

class UndefinedFunctionError : public RuntimeError {
public:
    explicit UndefinedFunctionError(const std::string& name)
        : RuntimeError("Undefined function: " + name) {}
};

class TypeMismatchError : public RuntimeError {
public:
    explicit TypeMismatchError(const std::string& message)
        : RuntimeError("Type mismatch: " + message) {}
};

} // namespace core
} // namespace runescript
