#include <gtest/gtest.h>
#include "core/ast.hpp"
#include <memory>

using namespace runescript::ast;

class ASTTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Setup test environment
    }
    
    void TearDown() override {
        // Cleanup
    }
};

// Test AST node creation
TEST_F(ASTTest, CreateStringExpr_ValidValue_CreatesCorrectNode) {
    auto expr = std::make_unique<StringExpr>("test string");
    
    ASSERT_TRUE(expr != nullptr);
    EXPECT_EQ(expr->value, "test string");
}

TEST_F(ASTTest, CreateVarExpr_ValidName_CreatesCorrectNode) {
    auto expr = std::make_unique<VarExpr>("variable_name");
    
    ASSERT_TRUE(expr != nullptr);
    EXPECT_EQ(expr->name, "variable_name");
}

TEST_F(ASTTest, CreateRegexExpr_ValidPattern_CreatesCorrectNode) {
    auto expr = std::make_unique<RegexExpr>("[0-9]+");
    
    ASSERT_TRUE(expr != nullptr);
    EXPECT_EQ(expr->pattern, "[0-9]+");
}

// Test function call AST
TEST_F(ASTTest, CreateCallExpr_ValidFunctionAndArgs_CreatesCorrectNode) {
    std::vector<std::unique_ptr<Expr>> args;
    args.push_back(std::make_unique<StringExpr>("argument1"));
    args.push_back(std::make_unique<StringExpr>("argument2"));
    
    auto call = std::make_unique<CallExpr>("function_name", std::move(args));
    
    ASSERT_TRUE(call != nullptr);
    EXPECT_EQ(call->callee, "function_name");
    EXPECT_EQ(call->args.size(), 2);
    
    auto* arg1 = dynamic_cast<StringExpr*>(call->args[0].get());
    auto* arg2 = dynamic_cast<StringExpr*>(call->args[1].get());
    
    ASSERT_TRUE(arg1 != nullptr);
    ASSERT_TRUE(arg2 != nullptr);
    EXPECT_EQ(arg1->value, "argument1");
    EXPECT_EQ(arg2->value, "argument2");
}

// Test pipeline AST
TEST_F(ASTTest, CreatePipeExpr_ValidLhsAndStages_CreatesCorrectNode) {
    auto lhs = std::make_unique<VarExpr>("input");
    
    std::vector<std::unique_ptr<Expr>> stages;
    stages.push_back(std::make_unique<CallExpr>("normalize", std::vector<std::unique_ptr<Expr>>{}));
    stages.push_back(std::make_unique<CallExpr>("tokenize", std::vector<std::unique_ptr<Expr>>{}));
    
    auto pipe = std::make_unique<PipeExpr>(std::move(lhs), std::move(stages));
    
    ASSERT_TRUE(pipe != nullptr);
    EXPECT_EQ(pipe->stages.size(), 2);
    
    auto* lhs_var = dynamic_cast<VarExpr*>(pipe->lhs.get());
    ASSERT_TRUE(lhs_var != nullptr);
    EXPECT_EQ(lhs_var->name, "input");
    
    auto* stage1 = dynamic_cast<CallExpr*>(pipe->stages[0].get());
    auto* stage2 = dynamic_cast<CallExpr*>(pipe->stages[1].get());
    
    ASSERT_TRUE(stage1 != nullptr);
    ASSERT_TRUE(stage2 != nullptr);
    EXPECT_EQ(stage1->callee, "normalize");
    EXPECT_EQ(stage2->callee, "tokenize");
}

// Test assignment AST
TEST_F(ASTTest, CreateAssignment_ValidNameAndValue_CreatesCorrectNode) {
    auto value = std::make_unique<StringExpr>("assigned value");
    auto assignment = std::make_unique<Assignment>("variable", std::move(value));
    
    ASSERT_TRUE(assignment != nullptr);
    EXPECT_EQ(assignment->var_name, "variable");
    
    auto* string_value = dynamic_cast<StringExpr*>(assignment->value.get());
    ASSERT_TRUE(string_value != nullptr);
    EXPECT_EQ(string_value->value, "assigned value");
}

// Test function definition AST
TEST_F(ASTTest, CreateFunctionDef_ValidComponents_CreatesCorrectNode) {
    std::vector<std::string> params = {"param1", "param2"};
    auto body = std::make_unique<StringExpr>("function body");
    
    auto func_def = std::make_unique<FunctionDef>("test_function", params, std::move(body));
    
    ASSERT_TRUE(func_def != nullptr);
    EXPECT_EQ(func_def->name, "test_function");
    EXPECT_EQ(func_def->params.size(), 2);
    EXPECT_EQ(func_def->params[0], "param1");
    EXPECT_EQ(func_def->params[1], "param2");
    
    auto* body_expr = dynamic_cast<StringExpr*>(func_def->body.get());
    ASSERT_TRUE(body_expr != nullptr);
    EXPECT_EQ(body_expr->value, "function body");
}

// Test group definition AST
TEST_F(ASTTest, CreateGroupDef_ValidNameAndCharacters_CreatesCorrectNode) {
    std::vector<std::string> characters = {"ㅏ", "ㅓ", "ㅗ", "ㅜ"};
    auto group_def = std::make_unique<GroupDef>("korean_vowels", characters);
    
    ASSERT_TRUE(group_def != nullptr);
    EXPECT_EQ(group_def->name, "korean_vowels");
    EXPECT_EQ(group_def->characters.size(), 4);
    EXPECT_EQ(group_def->characters[0], "ㅏ");
    EXPECT_EQ(group_def->characters[3], "ㅜ");
}

// Test rule definition AST
TEST_F(ASTTest, CreateRuleDef_ValidComponents_CreatesCorrectNode) {
    auto from_expr = std::make_unique<RegexExpr>("ㅎ+ㅣ");
    auto rule_def = std::make_unique<RuleDef>("palatalization", std::move(from_expr), "치");
    
    ASSERT_TRUE(rule_def != nullptr);
    EXPECT_EQ(rule_def->name, "palatalization");
    EXPECT_EQ(rule_def->to, "치");
    
    auto* from_regex = dynamic_cast<RegexExpr*>(rule_def->from.get());
    ASSERT_TRUE(from_regex != nullptr);
    EXPECT_EQ(from_regex->pattern, "ㅎ+ㅣ");
}

// Test unicode range definition AST
TEST_F(ASTTest, CreateUnicodeRangeDef_ValidRange_CreatesCorrectNode) {
    auto range_def = std::make_unique<UnicodeRangeDef>("hangul_syllables", 0xAC00, 0xD7AF);
    
    ASSERT_TRUE(range_def != nullptr);
    EXPECT_EQ(range_def->name, "hangul_syllables");
    EXPECT_EQ(range_def->start, 0xAC00);
    EXPECT_EQ(range_def->end, 0xD7AF);
}

// Test language profile AST
TEST_F(ASTTest, CreateLangProfile_ValidComponents_CreatesCorrectNode) {
    std::vector<std::string> used_groups = {"hangul_syllables", "korean_vowels", "korean_consonants"};
    auto lang_profile = std::make_unique<LangProfile>("ko-KR", used_groups);
    
    ASSERT_TRUE(lang_profile != nullptr);
    EXPECT_EQ(lang_profile->tag, "ko-KR");
    EXPECT_EQ(lang_profile->used_groups.size(), 3);
    EXPECT_EQ(lang_profile->used_groups[0], "hangul_syllables");
    EXPECT_EQ(lang_profile->used_groups[2], "korean_consonants");
}

// Test program AST
TEST_F(ASTTest, CreateProgram_MultipleStatements_CreatesCorrectNode) {
    auto program = std::make_unique<Program>();
    
    // Add some statements
    program->statements.push_back(std::make_unique<StringExpr>("statement1"));
    program->statements.push_back(std::make_unique<VarExpr>("variable"));
    program->statements.push_back(std::make_unique<Assignment>("x", std::make_unique<StringExpr>("value")));
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->statements.size(), 3);
    
    auto* stmt1 = dynamic_cast<StringExpr*>(program->statements[0].get());
    auto* stmt2 = dynamic_cast<VarExpr*>(program->statements[1].get());
    auto* stmt3 = dynamic_cast<Assignment*>(program->statements[2].get());
    
    ASSERT_TRUE(stmt1 != nullptr);
    ASSERT_TRUE(stmt2 != nullptr);
    ASSERT_TRUE(stmt3 != nullptr);
    
    EXPECT_EQ(stmt1->value, "statement1");
    EXPECT_EQ(stmt2->name, "variable");
    EXPECT_EQ(stmt3->var_name, "x");
}

// Test spell module AST
TEST_F(ASTTest, CreateSpellModule_MultipleFunctions_CreatesCorrectNode) {
    auto module = std::make_unique<SpellModule>();
    
    // Add some functions
    std::vector<std::string> params1 = {"text"};
    auto body1 = std::make_unique<StringExpr>("normalized");
    module->functions.emplace_back("normalize", params1, std::move(body1));
    
    std::vector<std::string> params2 = {"input", "delimiter"};
    auto body2 = std::make_unique<StringExpr>("tokenized");
    module->functions.emplace_back("tokenize", params2, std::move(body2));
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->functions.size(), 2);
    
    EXPECT_EQ(module->functions[0].name, "normalize");
    EXPECT_EQ(module->functions[0].params.size(), 1);
    EXPECT_EQ(module->functions[1].name, "tokenize");
    EXPECT_EQ(module->functions[1].params.size(), 2);
}

// Test rune module AST
TEST_F(ASTTest, CreateRuneModule_MultipleComponents_CreatesCorrectNode) {
    auto module = std::make_unique<RuneModule>();
    
    // Add group
    std::vector<std::string> chars = {"ㅏ", "ㅓ"};
    module->groups.emplace_back("vowels", chars);
    
    // Add rule
    auto from_expr = std::make_unique<StringExpr>("pattern");
    module->rules.emplace_back("rule1", std::move(from_expr), "replacement");
    
    // Add unicode range
    module->unicode_ranges.emplace_back("hangul", 0xAC00, 0xD7AF);
    
    // Add language profile
    std::vector<std::string> groups = {"vowels", "consonants"};
    module->language_profiles.emplace_back("ko-KR", groups);
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->groups.size(), 1);
    EXPECT_EQ(module->rules.size(), 1);
    EXPECT_EQ(module->unicode_ranges.size(), 1);
    EXPECT_EQ(module->language_profiles.size(), 1);
}

// Test caster program AST
TEST_F(ASTTest, CreateCasterProgram_MultipleDirectives_CreatesCorrectNode) {
    auto program = std::make_unique<CasterProgram>();
    
    program->load_directives.push_back("module1.spell");
    program->load_directives.push_back("module2.rune");
    program->run_directives.push_back("script1.rcs");
    program->backend = "cpp_runtime";
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->load_directives.size(), 2);
    EXPECT_EQ(program->run_directives.size(), 1);
    EXPECT_EQ(program->backend, "cpp_runtime");
    
    EXPECT_EQ(program->load_directives[0], "module1.spell");
    EXPECT_EQ(program->load_directives[1], "module2.rune");
    EXPECT_EQ(program->run_directives[0], "script1.rcs");
}
