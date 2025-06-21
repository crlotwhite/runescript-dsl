#include "ast.hpp"

namespace runescript {
namespace ast {

// StringExpr implementation
StringExpr::StringExpr(std::string val) : value(std::move(val)) {}

// RegexExpr implementation  
RegexExpr::RegexExpr(std::string pat) : pattern(std::move(pat)) {}

// VarExpr implementation
VarExpr::VarExpr(std::string n) : name(std::move(n)) {}

// CallExpr implementation
CallExpr::CallExpr(std::string c, std::vector<std::unique_ptr<Expr>> a)
    : callee(std::move(c)), args(std::move(a)) {}

// PipeExpr implementation
PipeExpr::PipeExpr(std::unique_ptr<Expr> l, std::vector<std::unique_ptr<Expr>> s)
    : lhs(std::move(l)), stages(std::move(s)) {}

// Assignment implementation
Assignment::Assignment(std::string name, std::unique_ptr<Expr> val)
    : var_name(std::move(name)), value(std::move(val)) {}

// FunctionDef implementation
FunctionDef::FunctionDef(std::string n, std::vector<std::string> p, std::unique_ptr<Expr> b)
    : name(std::move(n)), params(std::move(p)), body(std::move(b)) {}

// RuleDef implementation
RuleDef::RuleDef(std::string n, std::unique_ptr<Expr> f, std::string t)
    : name(std::move(n)), from(std::move(f)), to(std::move(t)) {}

// GroupDef implementation
GroupDef::GroupDef(std::string n, std::vector<std::string> chars)
    : name(std::move(n)), characters(std::move(chars)) {}

// UnicodeRangeDef implementation
UnicodeRangeDef::UnicodeRangeDef(std::string n, uint32_t s, uint32_t e)
    : name(std::move(n)), start(s), end(e) {}

// LangProfile implementation
LangProfile::LangProfile(std::string t, std::vector<std::string> groups)
    : tag(std::move(t)), used_groups(std::move(groups)) {}

} // namespace ast
} // namespace runescript
