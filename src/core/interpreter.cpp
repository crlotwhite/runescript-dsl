#include "interpreter.hpp"
#include "parser.hpp"
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <regex>

namespace runescript {
namespace core {

// Environment implementation
Environment::Environment(Environment* parent) : parent_(parent) {}

void Environment::define(const std::string& name, const Value& value) {
    variables_[name] = value;
}

Value Environment::get(const std::string& name) const {
    auto it = variables_.find(name);
    if (it != variables_.end()) {
        return it->second;
    }
    
    if (parent_) {
        return parent_->get(name);
    }
    
    throw UndefinedVariableError(name);
}

void Environment::set(const std::string& name, const Value& value) {
    auto it = variables_.find(name);
    if (it != variables_.end()) {
        it->second = value;
        return;
    }
    
    if (parent_) {
        try {
            parent_->set(name, value);
            return;
        } catch (const UndefinedVariableError&) {
            // Variable not found in parent, define in current scope
        }
    }
    
    variables_[name] = value;
}

// Interpreter implementation
Interpreter::Interpreter() {
    global_env_ = std::make_unique<Environment>();
    current_env_ = global_env_.get();
    initializeBuiltins();
}

void Interpreter::loadSpellModule(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw RuntimeError("Cannot open spell module: " + filename);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();
    
    Parser parser(content);
    auto module = parser.parseSpell();
    
    // Register functions from the module
    for (const auto& func : module->functions) {
        // Create a lambda that captures the function definition
        auto captured_func = func;
        registerBuiltin(func.name, [this, captured_func](const std::vector<Value>& args) -> Value {
            // Create new environment for function execution
            Environment func_env(current_env_);
            Environment* prev_env = current_env_;
            current_env_ = &func_env;
            
            // Bind parameters
            for (size_t i = 0; i < captured_func.params.size() && i < args.size(); ++i) {
                func_env.define(captured_func.params[i], args[i]);
            }
            
            // Execute function body
            Value result = evaluateExpression(*captured_func.body);
            
            // Restore environment
            current_env_ = prev_env;
            
            return result;
        });
    }
}

void Interpreter::loadRuneModule(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw RuntimeError("Cannot open rune module: " + filename);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();
    
    Parser parser(content);
    auto module = parser.parseRune();
    
    // Process rune definitions (character groups, rules, etc.)
    // This would typically involve setting up character classification
    // and phonetic transformation rules for the runtime
}

Value Interpreter::execute(const ast::Program& program) {
    Value last_result = std::string("");
    
    for (const auto& stmt : program.statements) {
        last_result = evaluateExpression(*stmt);
    }
    
    return last_result;
}

Value Interpreter::evaluateExpression(const ast::Expr& expr) {
    // Dynamic dispatch based on expression type
    if (auto str_expr = dynamic_cast<const ast::StringExpr*>(&expr)) {
        return str_expr->value;
    }
    
    if (auto var_expr = dynamic_cast<const ast::VarExpr*>(&expr)) {
        return current_env_->get(var_expr->name);
    }
    
    if (auto assignment = dynamic_cast<const ast::Assignment*>(&expr)) {
        return evaluateAssignment(*assignment);
    }
    
    if (auto call_expr = dynamic_cast<const ast::CallExpr*>(&expr)) {
        return evaluateCall(*call_expr);
    }
    
    if (auto pipe_expr = dynamic_cast<const ast::PipeExpr*>(&expr)) {
        return evaluatePipe(*pipe_expr);
    }
    
    throw RuntimeError("Unknown expression type");
}

void Interpreter::setVariable(const std::string& name, const Value& value) {
    current_env_->define(name, value);
}

Value Interpreter::getVariable(const std::string& name) const {
    return current_env_->get(name);
}

void Interpreter::registerBuiltin(const std::string& name, BuiltinFunction func) {
    builtins_[name] = func;
}

void Interpreter::initializeBuiltins() {
    // Text processing functions
    registerBuiltin("normalize", [this](const std::vector<Value>& args) -> Value {
        if (args.empty()) {
            throw RuntimeError("normalize() requires at least one argument");
        }
        
        std::string text = valueToString(args[0]);
        
        // Basic normalization: trim whitespace, convert to lowercase
        text.erase(0, text.find_first_not_of(" \t\n\r"));
        text.erase(text.find_last_not_of(" \t\n\r") + 1);
        std::transform(text.begin(), text.end(), text.begin(), ::tolower);
        
        // Remove extra whitespace
        text = std::regex_replace(text, std::regex("\\s+"), " ");
        
        return text;
    });
    
    registerBuiltin("tokenize", [this](const std::vector<Value>& args) -> Value {
        if (args.empty()) {
            throw RuntimeError("tokenize() requires at least one argument");
        }
        
        std::string text = valueToString(args[0]);
        std::string delimiter = args.size() > 1 ? valueToString(args[1]) : " ";
        
        std::vector<std::string> tokens;
        size_t start = 0;
        size_t end = text.find(delimiter);
        
        while (end != std::string::npos) {
            std::string token = text.substr(start, end - start);
            if (!token.empty()) {
                tokens.push_back(token);
            }
            start = end + delimiter.length();
            end = text.find(delimiter, start);
        }
        
        std::string last_token = text.substr(start);
        if (!last_token.empty()) {
            tokens.push_back(last_token);
        }
        
        return tokens;
    });
    
    registerBuiltin("remove_punctuation", [this](const std::vector<Value>& args) -> Value {
        if (args.empty()) {
            throw RuntimeError("remove_punctuation() requires one argument");
        }
        
        std::string text = valueToString(args[0]);
        text = std::regex_replace(text, std::regex("[[:punct:]]"), "");
        
        return text;
    });
    
    registerBuiltin("korean_g2p", [this](const std::vector<Value>& args) -> Value {
        if (args.empty()) {
            throw RuntimeError("korean_g2p() requires one argument");
        }
        
        std::string text = valueToString(args[0]);
        
        // Placeholder Korean G2P implementation
        // In a real implementation, this would use proper Korean phonetic rules
        std::string result = text;
        
        // Basic Korean character transliteration (very simplified)
        result = std::regex_replace(result, std::regex("\uc548"), "an");
        result = std::regex_replace(result, std::regex("\ub155"), "nyeong");
        result = std::regex_replace(result, std::regex("\ud558"), "ha");
        result = std::regex_replace(result, std::regex("\uc138"), "se");
        result = std::regex_replace(result, std::regex("\uc694"), "yo");
        
        return result;
    });
    
    registerBuiltin("phonemize", [this](const std::vector<Value>& args) -> Value {
        if (args.empty()) {
            throw RuntimeError("phonemize() requires one argument");
        }
        
        std::string text = valueToString(args[0]);
        
        // Placeholder phonemization
        // Real implementation would use language-specific phonetic rules
        return text + " [phonemized]";
    });
    
    registerBuiltin("length", [this](const std::vector<Value>& args) -> Value {
        if (args.empty()) {
            throw RuntimeError("length() requires one argument");
        }
        
        if (std::holds_alternative<std::string>(args[0])) {
            return static_cast<double>(std::get<std::string>(args[0]).length());
        } else if (std::holds_alternative<std::vector<std::string>>(args[0])) {
            return static_cast<double>(std::get<std::vector<std::string>>(args[0]).size());
        }
        
        throw TypeMismatchError("length() requires string or list argument");
    });
    
    registerBuiltin("output", [this](const std::vector<Value>& args) -> Value {
        for (const auto& arg : args) {
            std::cout << valueToString(arg) << " ";
        }
        std::cout << std::endl;
        return std::string("");
    });
}

Value Interpreter::callFunction(const std::string& name, const std::vector<Value>& args) {
    auto it = builtins_.find(name);
    if (it != builtins_.end()) {
        return it->second(args);
    }
    
    throw UndefinedFunctionError(name);
}

Value Interpreter::evaluateAssignment(const ast::Assignment& assignment) {
    Value value = evaluateExpression(*assignment.value);
    current_env_->define(assignment.var_name, value);
    return value;
}

Value Interpreter::evaluateCall(const ast::CallExpr& call) {
    std::vector<Value> args;
    for (const auto& arg : call.args) {
        args.push_back(evaluateExpression(*arg));
    }
    
    return callFunction(call.callee, args);
}

Value Interpreter::evaluatePipe(const ast::PipeExpr& pipe) {
    Value current = evaluateExpression(*pipe.lhs);
    
    for (const auto& stage : pipe.stages) {
        if (auto call_expr = dynamic_cast<const ast::CallExpr*>(stage.get())) {
            std::vector<Value> args = {current};
            for (const auto& arg : call_expr->args) {
                args.push_back(evaluateExpression(*arg));
            }
            current = callFunction(call_expr->callee, args);
        } else {
            throw RuntimeError("Pipeline stage must be a function call");
        }
    }
    
    return current;
}

std::string Interpreter::valueToString(const Value& value) const {
    if (std::holds_alternative<std::string>(value)) {
        return std::get<std::string>(value);
    } else if (std::holds_alternative<double>(value)) {
        return std::to_string(std::get<double>(value));
    } else if (std::holds_alternative<bool>(value)) {
        return std::get<bool>(value) ? "true" : "false";
    } else if (std::holds_alternative<std::vector<std::string>>(value)) {
        const auto& vec = std::get<std::vector<std::string>>(value);
        std::string result = "[";
        for (size_t i = 0; i < vec.size(); ++i) {
            if (i > 0) result += ", ";
            result += "\"" + vec[i] + "\"";
        }
        result += "]";
        return result;
    }
    
    return "";
}

double Interpreter::valueToNumber(const Value& value) const {
    if (std::holds_alternative<double>(value)) {
        return std::get<double>(value);
    } else if (std::holds_alternative<std::string>(value)) {
        try {
            return std::stod(std::get<std::string>(value));
        } catch (...) {
            throw TypeMismatchError("Cannot convert string to number");
        }
    }
    
    throw TypeMismatchError("Cannot convert value to number");
}

bool Interpreter::valueToBool(const Value& value) const {
    if (std::holds_alternative<bool>(value)) {
        return std::get<bool>(value);
    } else if (std::holds_alternative<std::string>(value)) {
        const std::string& str = std::get<std::string>(value);
        return !str.empty() && str != "false" && str != "0";
    } else if (std::holds_alternative<double>(value)) {
        return std::get<double>(value) != 0.0;
    }
    
    return false;
}

} // namespace core
} // namespace runescript
