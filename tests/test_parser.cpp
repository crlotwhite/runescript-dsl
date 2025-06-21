#include <gtest/gtest.h>
#include "core/parser.hpp"
#include "core/ast.hpp"
#include <memory>
#include <string>

using namespace runescript::core;
using namespace runescript::ast;

class ParserTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Setup test environment
    }
    
    void TearDown() override {
        // Cleanup
    }
};

// Test basic RCS parsing
TEST_F(ParserTest, ParseRCS_SimpleAssignment_ReturnsValidAST) {
    std::string source = R"(
        x = "hello world"
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->statements.size(), 1);
    
    auto* assignment = dynamic_cast<Assignment*>(program->statements[0].get());
    ASSERT_TRUE(assignment != nullptr);
    EXPECT_EQ(assignment->var_name, "x");
    
    auto* string_expr = dynamic_cast<StringExpr*>(assignment->value.get());
    ASSERT_TRUE(string_expr != nullptr);
    EXPECT_EQ(string_expr->value, "hello world");
}

TEST_F(ParserTest, ParseRCS_PipelineExpression_ReturnsValidAST) {
    std::string source = R"(
        result = input |> normalize() |> tokenize()
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->statements.size(), 1);
    
    auto* assignment = dynamic_cast<Assignment*>(program->statements[0].get());
    ASSERT_TRUE(assignment != nullptr);
    EXPECT_EQ(assignment->var_name, "result");
    
    // Check if the value is a pipeline expression
    auto* pipe_expr = dynamic_cast<PipeExpr*>(assignment->value.get());
    ASSERT_TRUE(pipe_expr != nullptr);
    EXPECT_EQ(pipe_expr->stages.size(), 2);
}

TEST_F(ParserTest, ParseRCS_MultipleStatements_ReturnsValidAST) {
    std::string source = R"(
        input = "test input"
        processed = input |> normalize()
        output processed
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->statements.size(), 3);
}

TEST_F(ParserTest, ParseRCS_WithComments_IgnoresComments) {
    std::string source = R"(
        // This is a comment
        x = "hello" // Another comment
        // Final comment
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->statements.size(), 1);
}

// Test Spell parsing
TEST_F(ParserTest, ParseSpell_SimpleFunctionDef_ReturnsValidAST) {
    std::string source = R"(
        def greet(name) = "Hello, " + name
    )";
    
    Parser parser(source);
    auto module = parser.parseSpell();
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->functions.size(), 1);
    
    const auto& func = module->functions[0];
    EXPECT_EQ(func.name, "greet");
    EXPECT_EQ(func.params.size(), 1);
    EXPECT_EQ(func.params[0], "name");
}

TEST_F(ParserTest, ParseSpell_MultipleFunctions_ReturnsValidAST) {
    std::string source = R"(
        def normalize_text(text) = text |> trim() |> to_lower()
        def tokenize_text(text) = text |> split(" ")
    )";
    
    Parser parser(source);
    auto module = parser.parseSpell();
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->functions.size(), 2);
    
    EXPECT_EQ(module->functions[0].name, "normalize_text");
    EXPECT_EQ(module->functions[1].name, "tokenize_text");
}

// Test Rune parsing
TEST_F(ParserTest, ParseRune_GroupDefinition_ReturnsValidAST) {
    std::string source = R"(
        group korean_vowels = ["ㅏ", "ㅓ", "ㅗ", "ㅜ"]
    )";
    
    Parser parser(source);
    auto module = parser.parseRune();
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->groups.size(), 1);
    
    const auto& group = module->groups[0];
    EXPECT_EQ(group.name, "korean_vowels");
    // Note: The current parser implementation needs enhancement for character lists
}

TEST_F(ParserTest, ParseRune_UnicodeRangeDefinition_ReturnsValidAST) {
    std::string source = R"(
        define block hangul_syllables = U+AC00 .. U+D7AF
    )";
    
    Parser parser(source);
    auto module = parser.parseRune();
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->unicode_ranges.size(), 1);
    
    const auto& range = module->unicode_ranges[0];
    EXPECT_EQ(range.name, "hangul_syllables");
}

// Test Caster parsing
TEST_F(ParserTest, ParseCaster_LoadDirectives_ReturnsValidAST) {
    std::string source = R"(
        load "korean_g2p.spell"
        load "korean.rune"
        use_backend "cpp_runtime"
    )";
    
    Parser parser(source);
    auto program = parser.parseCaster();
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->load_directives.size(), 2);
    EXPECT_EQ(program->backend, "cpp_runtime");
}

// Error handling tests
TEST_F(ParserTest, ParseRCS_InvalidSyntax_HandlesGracefully) {
    std::string source = R"(
        x = 
        y = "incomplete
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    
    // The parser should not crash, but may return incomplete AST
    // This test ensures robustness
    ASSERT_TRUE(program != nullptr);
}

TEST_F(ParserTest, ParseSpell_MissingEqualsSign_HandlesGracefully) {
    std::string source = R"(
        def broken_function(x) "missing equals"
    )";
    
    Parser parser(source);
    auto module = parser.parseSpell();
    
    // Should handle gracefully without crashing
    ASSERT_TRUE(module != nullptr);
}

// Integration tests with complex examples
TEST_F(ParserTest, ParseRCS_ComplexPipeline_ReturnsValidAST) {
    std::string source = R"(
        input = "안녕하세요, 세계!"
        result = input 
          |> normalize() 
          |> korean_g2p() 
          |> phonemize("ko-KR")
          |> format_output()
        output result
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    
    ASSERT_TRUE(program != nullptr);
    EXPECT_EQ(program->statements.size(), 3);
    
    // Check the pipeline structure
    auto* assignment = dynamic_cast<Assignment*>(program->statements[1].get());
    ASSERT_TRUE(assignment != nullptr);
    
    auto* pipe_expr = dynamic_cast<PipeExpr*>(assignment->value.get());
    ASSERT_TRUE(pipe_expr != nullptr);
    EXPECT_EQ(pipe_expr->stages.size(), 4);
}

TEST_F(ParserTest, ParseSpell_KoreanG2PFunction_ReturnsValidAST) {
    std::string source = R"(
        def korean_g2p(text) = 
          text |> hangul_to_jamo() |> apply_phonetic_rules()
        
        def hangul_to_jamo(text) = decompose_hangul(text)
    )";
    
    Parser parser(source);
    auto module = parser.parseSpell();
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->functions.size(), 2);
    
    const auto& korean_g2p = module->functions[0];
    EXPECT_EQ(korean_g2p.name, "korean_g2p");
    EXPECT_EQ(korean_g2p.params.size(), 1);
    EXPECT_EQ(korean_g2p.params[0], "text");
}
