#include "parser.hpp"
#include <iostream>
#include <cctype>
#include <stdexcept>

namespace runescript {
namespace core {

Parser::Parser(const std::string& source) 
    : source_(source), position_(0) {
}

std::unique_ptr<ast::Program> Parser::parseRCS() {
    auto program = std::make_unique<ast::Program>();
    
    while (position_ < source_.length()) {
        skipWhitespace();
        if (position_ >= source_.length()) break;
        
        // Skip comments
        if (match("//")) {
            while (position_ < source_.length() && source_[position_] != '\n') {
                position_++;
            }
            continue;
        }
        
        // Parse statement
        auto stmt = parseStatement();
        if (stmt) {
            program->statements.push_back(std::move(stmt));
        }
    }
    
    return program;
}

std::unique_ptr<ast::SpellModule> Parser::parseSpell() {
    auto module = std::make_unique<ast::SpellModule>();
    
    while (position_ < source_.length()) {
        skipWhitespace();
        if (position_ >= source_.length()) break;
        
        // Skip comments
        if (match("//")) {
            while (position_ < source_.length() && source_[position_] != '\n') {
                position_++;
            }
            continue;
        }
        
        // Parse function definition
        if (match("def")) {
            auto func = parseFunctionDef();
            if (func) {
                module->functions.push_back(std::move(*func));
            }
        } else if (match("import")) {
            // Handle import statements (implementation needed)
            parseImport();
        }
    }
    
    return module;
}

std::unique_ptr<ast::RuneModule> Parser::parseRune() {
    auto module = std::make_unique<ast::RuneModule>();
    
    while (position_ < source_.length()) {
        skipWhitespace();
        if (position_ >= source_.length()) break;
        
        // Skip comments
        if (match("//")) {
            while (position_ < source_.length() && source_[position_] != '\n') {
                position_++;
            }
            continue;
        }
        
        // Parse rune statements
        if (match("group")) {
            auto group = parseGroupDef();
            if (group) {
                module->groups.push_back(std::move(*group));
            }
        } else if (match("rule")) {
            auto rule = parseRuleDef();
            if (rule) {
                module->rules.push_back(std::move(*rule));
            }
        } else if (match("define block")) {
            auto range = parseUnicodeRangeDef();
            if (range) {
                module->unicode_ranges.push_back(std::move(*range));
            }
        } else if (match("lang")) {
            auto lang = parseLangProfile();
            if (lang) {
                module->language_profiles.push_back(std::move(*lang));
            }
        }
    }
    
    return module;
}

std::unique_ptr<ast::CasterProgram> Parser::parseCaster() {
    auto program = std::make_unique<ast::CasterProgram>();
    
    while (position_ < source_.length()) {
        skipWhitespace();
        if (position_ >= source_.length()) break;
        
        // Skip comments
        if (match("//")) {
            while (position_ < source_.length() && source_[position_] != '\n') {
                position_++;
            }
            continue;
        }
        
        // Parse directives
        if (match("load")) {
            skipWhitespace();
            std::string module = parseString();
            program->load_directives.push_back(module);
        } else if (match("run")) {
            skipWhitespace();
            std::string script = parseString();
            program->run_directives.push_back(script);
        } else if (match("use_backend")) {
            skipWhitespace();
            program->backend = parseString();
        }
        // Additional directive parsing will be implemented
    }
    
    return program;
}

void Parser::skipWhitespace() {
    while (position_ < source_.length() && 
           (std::isspace(source_[position_]) || source_[position_] == '\n')) {
        position_++;
    }
}

bool Parser::match(const std::string& expected) {
    if (position_ + expected.length() > source_.length()) {
        return false;
    }
    
    if (source_.substr(position_, expected.length()) == expected) {
        position_ += expected.length();
        return true;
    }
    
    return false;
}

std::string Parser::parseIdentifier() {
    std::string identifier;
    
    if (position_ < source_.length() && 
        (std::isalpha(source_[position_]) || source_[position_] == '_')) {
        identifier += source_[position_++];
        
        while (position_ < source_.length() && 
               (std::isalnum(source_[position_]) || source_[position_] == '_')) {
            identifier += source_[position_++];
        }
    }
    
    return identifier;
}

std::string Parser::parseString() {
    std::string result;
    
    if (position_ < source_.length() && source_[position_] == '"') {
        position_++; // Skip opening quote
        
        while (position_ < source_.length() && source_[position_] != '"') {
            if (source_[position_] == '\\' && position_ + 1 < source_.length()) {
                // Handle escape sequences
                position_++;
                switch (source_[position_]) {
                    case 'n': result += '\n'; break;
                    case 't': result += '\t'; break;
                    case 'r': result += '\r'; break;
                    case '\\': result += '\\'; break;
                    case '"': result += '"'; break;
                    default: result += source_[position_]; break;
                }
                position_++;
            } else {
                result += source_[position_++];
            }
        }
        
        if (position_ < source_.length() && source_[position_] == '"') {
            position_++; // Skip closing quote
        }
    }
    
    return result;
}

// Helper parsing methods (basic implementations)
std::unique_ptr<ast::Expr> Parser::parseStatement() {
    skipWhitespace();
    
    // Check for assignment
    std::string identifier = parseIdentifier();
    if (!identifier.empty()) {
        skipWhitespace();
        if (match("=")) {
            skipWhitespace();
            auto value = parseExpression();
            return std::make_unique<ast::Assignment>(identifier, std::move(value));
        }
    }
    
    // Reset position if not assignment
    position_ -= identifier.length();
    return parseExpression();
}

std::unique_ptr<ast::Expr> Parser::parseExpression() {
    // Basic expression parsing (to be expanded)
    skipWhitespace();
    
    if (position_ < source_.length() && source_[position_] == '"') {
        std::string str = parseString();
        return std::make_unique<ast::StringExpr>(str);
    }
    
    std::string identifier = parseIdentifier();
    if (!identifier.empty()) {
        return std::make_unique<ast::VarExpr>(identifier);
    }
    
    return nullptr;
}

std::unique_ptr<ast::FunctionDef> Parser::parseFunctionDef() {
    skipWhitespace();
    std::string name = parseIdentifier();
    
    skipWhitespace();
    if (!match("(")) return nullptr;
    
    std::vector<std::string> params;
    // Parse parameters (basic implementation)
    
    skipWhitespace();
    if (!match(")")) return nullptr;
    
    skipWhitespace();
    if (!match("=")) return nullptr;
    
    skipWhitespace();
    auto body = parseExpression();
    
    return std::make_unique<ast::FunctionDef>(name, params, std::move(body));
}

std::unique_ptr<ast::GroupDef> Parser::parseGroupDef() {
    // Basic group definition parsing
    skipWhitespace();
    std::string name = parseIdentifier();
    
    skipWhitespace();
    if (!match("=")) return nullptr;
    
    std::vector<std::string> characters;
    // Parse character list (basic implementation)
    
    return std::make_unique<ast::GroupDef>(name, characters);
}

std::unique_ptr<ast::RuleDef> Parser::parseRuleDef() {
    // Basic rule definition parsing
    skipWhitespace();
    std::string name = parseIdentifier();
    
    std::vector<std::string> empty_vec;
    auto from_expr = std::make_unique<ast::StringExpr>("");
    
    return std::make_unique<ast::RuleDef>(name, std::move(from_expr), "");
}

std::unique_ptr<ast::UnicodeRangeDef> Parser::parseUnicodeRangeDef() {
    // Basic unicode range parsing
    skipWhitespace();
    std::string name = parseIdentifier();
    
    return std::make_unique<ast::UnicodeRangeDef>(name, 0, 0);
}

std::unique_ptr<ast::LangProfile> Parser::parseLangProfile() {
    // Basic language profile parsing
    skipWhitespace();
    std::string tag = parseString();
    
    std::vector<std::string> groups;
    
    return std::make_unique<ast::LangProfile>(tag, groups);
}

void Parser::parseImport() {
    // Basic import parsing
    skipWhitespace();
    std::string module = parseString();
    // Store import information
}

} // namespace core
} // namespace runescript
