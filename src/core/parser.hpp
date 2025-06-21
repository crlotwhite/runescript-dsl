#pragma once

#include <memory>
#include <string>
#include <vector>
#include "ast.hpp"

namespace runescript {
namespace core {

class Parser {
public:
    explicit Parser(const std::string& source);
    
    // Parse different DSL file types
    std::unique_ptr<ast::Program> parseRCS();
    std::unique_ptr<ast::SpellModule> parseSpell();
    std::unique_ptr<ast::RuneModule> parseRune();
    std::unique_ptr<ast::CasterProgram> parseCaster();
    
private:
    std::string source_;
    size_t position_;
    
    // Helper parsing methods
    void skipWhitespace();
    bool match(const std::string& expected);
    std::string parseIdentifier();
    std::string parseString();
};

} // namespace core
} // namespace runescript
