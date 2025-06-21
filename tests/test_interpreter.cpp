#include <gtest/gtest.h>
#include "core/interpreter.hpp"
#include "core/parser.hpp"
#include <variant>

using namespace runescript::core;

class InterpreterTest : public ::testing::Test {
protected:
    void SetUp() override {
        interpreter = std::make_unique<Interpreter>();
    }
    
    void TearDown() override {
        interpreter.reset();
    }
    
    std::unique_ptr<Interpreter> interpreter;
};

// Test basic variable assignment and retrieval
TEST_F(InterpreterTest, SetAndGetVariable_StringValue_ReturnsCorrectValue) {
    interpreter->setVariable("test_var", std::string("hello"));
    Value result = interpreter->getVariable("test_var");
    
    ASSERT_TRUE(std::holds_alternative<std::string>(result));
    EXPECT_EQ(std::get<std::string>(result), "hello");
}

TEST_F(InterpreterTest, SetAndGetVariable_NumberValue_ReturnsCorrectValue) {
    interpreter->setVariable("number_var", 42.0);
    Value result = interpreter->getVariable("number_var");
    
    ASSERT_TRUE(std::holds_alternative<double>(result));
    EXPECT_EQ(std::get<double>(result), 42.0);
}

// Test built-in functions
TEST_F(InterpreterTest, ExecuteNormalize_ValidInput_ReturnsNormalizedText) {
    std::string source = R"(
        input = "  Hello, WORLD!  "
        result = input |> normalize()
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    Value result = interpreter->execute(*program);
    
    // The last statement should be the assignment to result
    Value final_result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<std::string>(final_result));
    
    std::string normalized = std::get<std::string>(final_result);
    EXPECT_EQ(normalized, "hello, world!");
}

TEST_F(InterpreterTest, ExecuteTokenize_ValidInput_ReturnsTokenList) {
    std::string source = R"(
        input = "apple,banana,cherry"
        result = input |> tokenize(",")
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<std::vector<std::string>>(result));
    
    auto tokens = std::get<std::vector<std::string>>(result);
    EXPECT_EQ(tokens.size(), 3);
    EXPECT_EQ(tokens[0], "apple");
    EXPECT_EQ(tokens[1], "banana");
    EXPECT_EQ(tokens[2], "cherry");
}

TEST_F(InterpreterTest, ExecuteKoreanG2P_ValidInput_ReturnsPhonemes) {
    std::string source = R"(
        input = "안녕"
        result = input |> korean_g2p()
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<std::string>(result));
    
    std::string phonemes = std::get<std::string>(result);
    EXPECT_EQ(phonemes, "annyeong"); // Based on the simplified implementation
}

// Test pipeline execution
TEST_F(InterpreterTest, ExecutePipeline_MultipleStages_ReturnsCorrectResult) {
    std::string source = R"(
        input = "  Hello, WORLD!  "
        result = input |> normalize() |> remove_punctuation()
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<std::string>(result));
    
    std::string processed = std::get<std::string>(result);
    EXPECT_EQ(processed, "hello world");
}

TEST_F(InterpreterTest, ExecutePipeline_KoreanTextProcessing_ReturnsCorrectResult) {
    std::string source = R"(
        input = "안녕하세요"
        result = input |> korean_g2p() |> phonemize()
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<std::string>(result));
    
    std::string phonemized = std::get<std::string>(result);
    // Should contain both G2P and phonemization results
    EXPECT_TRUE(phonemized.find("annyeong") != std::string::npos);
    EXPECT_TRUE(phonemized.find("[phonemized]") != std::string::npos);
}

// Test function calls with multiple arguments
TEST_F(InterpreterTest, ExecuteFunctionCall_MultipleArguments_ReturnsCorrectResult) {
    std::string source = R"(
        text = "hello world hello"
        result = tokenize(text, " ")
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<std::vector<std::string>>(result));
    
    auto tokens = std::get<std::vector<std::string>>(result);
    EXPECT_EQ(tokens.size(), 3);
    EXPECT_EQ(tokens[0], "hello");
    EXPECT_EQ(tokens[1], "world");
    EXPECT_EQ(tokens[2], "hello");
}

// Test length function
TEST_F(InterpreterTest, ExecuteLength_StringInput_ReturnsCorrectLength) {
    std::string source = R"(
        text = "hello"
        result = length(text)
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<double>(result));
    EXPECT_EQ(std::get<double>(result), 5.0);
}

TEST_F(InterpreterTest, ExecuteLength_ListInput_ReturnsCorrectLength) {
    std::string source = R"(
        text = "a,b,c,d"
        tokens = tokenize(text, ",")
        result = length(tokens)
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<double>(result));
    EXPECT_EQ(std::get<double>(result), 4.0);
}

// Test spell module loading
TEST_F(InterpreterTest, LoadSpellModule_ValidModule_LoadsFunctionsSuccessfully) {
    // Create a temporary spell file content
    std::string spell_content = R"(
        def greet(name) = "Hello, " + name
        def double_text(text) = text + text
    )";
    
    // Note: In a real test, we would write this to a file
    // For now, we'll test the parsing part
    Parser parser(spell_content);
    auto module = parser.parseSpell();
    
    ASSERT_TRUE(module != nullptr);
    EXPECT_EQ(module->functions.size(), 2);
}

// Error handling tests
TEST_F(InterpreterTest, GetUndefinedVariable_ThrowsException) {
    EXPECT_THROW(interpreter->getVariable("undefined_var"), UndefinedVariableError);
}

TEST_F(InterpreterTest, CallUndefinedFunction_ThrowsException) {
    std::string source = R"(
        result = undefined_function("test")
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    EXPECT_THROW(interpreter->execute(*program), UndefinedFunctionError);
}

TEST_F(InterpreterTest, CallFunctionWithWrongArgumentCount_HandlesGracefully) {
    std::string source = R"(
        result = tokenize() // Missing required argument
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    EXPECT_THROW(interpreter->execute(*program), RuntimeError);
}

// Complex integration tests
TEST_F(InterpreterTest, ExecuteComplexPipeline_MultipleOperations_ReturnsCorrectResult) {
    std::string source = R"(
        input = "  안녕하세요, 세계!  "
        normalized = input |> normalize()
        without_punct = normalized |> remove_punctuation()
        phonemes = without_punct |> korean_g2p()
        final_result = phonemes |> phonemize()
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    // Check intermediate results
    Value normalized = interpreter->getVariable("normalized");
    ASSERT_TRUE(std::holds_alternative<std::string>(normalized));
    
    Value without_punct = interpreter->getVariable("without_punct");
    ASSERT_TRUE(std::holds_alternative<std::string>(without_punct));
    
    Value final_result = interpreter->getVariable("final_result");
    ASSERT_TRUE(std::holds_alternative<std::string>(final_result));
    
    std::string result_str = std::get<std::string>(final_result);
    EXPECT_FALSE(result_str.empty());
    EXPECT_TRUE(result_str.find("annyeong") != std::string::npos);
}

TEST_F(InterpreterTest, ExecuteNestedFunctionCalls_ReturnsCorrectResult) {
    std::string source = R"(
        text = "hello world"
        result = length(tokenize(text, " "))
    )";
    
    Parser parser(source);
    auto program = parser.parseRCS();
    ASSERT_TRUE(program != nullptr);
    
    interpreter->execute(*program);
    
    Value result = interpreter->getVariable("result");
    ASSERT_TRUE(std::holds_alternative<double>(result));
    EXPECT_EQ(std::get<double>(result), 2.0);
}
