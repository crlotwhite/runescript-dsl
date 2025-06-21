#pragma once

#include <memory>
#include <string>
#include <vector>

namespace runescript {
namespace ast {

// Base expression types
struct Expr {
    virtual ~Expr() = default;
};

struct StringExpr : Expr {
    std::string value;
    explicit StringExpr(std::string val) : value(std::move(val)) {}
};

struct RegexExpr : Expr {
    std::string pattern;
    explicit RegexExpr(std::string pat) : pattern(std::move(pat)) {}
};

struct VarExpr : Expr {
    std::string name;
    explicit VarExpr(std::string n) : name(std::move(n)) {}
};

struct CallExpr : Expr {
    std::string callee;
    std::vector<std::unique_ptr<Expr>> args;
    
    CallExpr(std::string c, std::vector<std::unique_ptr<Expr>> a)
        : callee(std::move(c)), args(std::move(a)) {}
};

struct PipeExpr : Expr {
    std::unique_ptr<Expr> lhs;
    std::vector<std::unique_ptr<Expr>> stages;
    
    PipeExpr(std::unique_ptr<Expr> l, std::vector<std::unique_ptr<Expr>> s)
        : lhs(std::move(l)), stages(std::move(s)) {}
};

struct Assignment : Expr {
    std::string var_name;
    std::unique_ptr<Expr> value;
    
    Assignment(std::string name, std::unique_ptr<Expr> val)
        : var_name(std::move(name)), value(std::move(val)) {}
};

// Function and rule definitions
struct FunctionDef {
    std::string name;
    std::vector<std::string> params;
    std::unique_ptr<Expr> body;
    
    FunctionDef(std::string n, std::vector<std::string> p, std::unique_ptr<Expr> b)
        : name(std::move(n)), params(std::move(p)), body(std::move(b)) {}
};

struct RuleDef {
    std::string name;
    std::unique_ptr<Expr> from;
    std::string to;
    
    RuleDef(std::string n, std::unique_ptr<Expr> f, std::string t)
        : name(std::move(n)), from(std::move(f)), to(std::move(t)) {}
};

struct GroupDef {
    std::string name;
    std::vector<std::string> characters;
    
    GroupDef(std::string n, std::vector<std::string> chars)
        : name(std::move(n)), characters(std::move(chars)) {}
};

struct UnicodeRangeDef {
    std::string name;
    uint32_t start, end;
    
    UnicodeRangeDef(std::string n, uint32_t s, uint32_t e)
        : name(std::move(n)), start(s), end(e) {}
};

struct LangProfile {
    std::string tag;
    std::vector<std::string> used_groups;
    
    LangProfile(std::string t, std::vector<std::string> groups)
        : tag(std::move(t)), used_groups(std::move(groups)) {}
};

// Top-level program structures
struct Program {
    std::vector<std::unique_ptr<Expr>> statements;
};

struct SpellModule {
    std::vector<FunctionDef> functions;
};

struct RuneModule {
    std::vector<GroupDef> groups;
    std::vector<RuleDef> rules;
    std::vector<UnicodeRangeDef> unicode_ranges;
    std::vector<LangProfile> language_profiles;
};

struct CasterProgram {
    std::vector<std::string> load_directives;
    std::vector<std::string> run_directives;
    std::string backend;
    // Additional configuration will be added later
};

} // namespace ast
} // namespace runescript
