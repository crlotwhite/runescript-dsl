{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import SpellChecker.Parser
import SpellChecker.TypeInfer
import SpellChecker.AST
import SpellChecker.Optimizer

main :: IO ()
main = hspec $ do
  describe "SpellChecker.Parser" $ do
    it "parses simple string expressions" $ do
      parseExpr "\"hello\"" `shouldSatisfy` isRight
    
    it "parses variable expressions" $ do
      parseExpr "variable_name" `shouldSatisfy` isRight
    
    it "parses function calls" $ do
      parseExpr "normalize(\"text\")" `shouldSatisfy` isRight
    
    it "parses pipeline expressions" $ do
      parseExpr "\"text\" |> normalize() |> tokenize()" `shouldSatisfy` isRight
    
    it "parses function definitions" $ do
      parseFunDef "def greet(name) = \"Hello, \" + name" `shouldSatisfy` isRight
    
    it "handles parse errors gracefully" $ do
      parseExpr "unclosed \"string" `shouldSatisfy` isLeft
  
  describe "SpellChecker.TypeInfer" $ do
    it "infers string type for string literals" $ do
      inferType [] (EString () "hello") `shouldBe` Right TText
    
    it "infers regex type for regex literals" $ do
      inferType [] (ERegex () "[0-9]+") `shouldBe` Right TRegex
    
    it "fails for unbound variables" $ do
      inferType [] (EVar () "unbound") `shouldSatisfy` isLeft
    
    it "infers function types correctly" $ do
      let env = [("normalize", TFun TText TText)]
      inferType env (ECall () "normalize" [EString () "text"]) `shouldBe` Right TText
    
    it "handles pipeline type inference" $ do
      let env = [("normalize", TFun TText TText), ("tokenize", TFun TText (TVar "List"))]
      let pipeline = EPipe () (EString () "text") [ECall () "normalize" [], ECall () "tokenize" []]
      inferType env pipeline `shouldSatisfy` isRight
  
  describe "SpellChecker.Optimizer" $ do
    it "performs constant folding" $ do
      let expr = ECall () "concat" [EString () "hello", EString () " world"]
      let (optimized, stats) = runOpt defaultOptConfig (constantFoldExpr expr)
      constantsFolded stats `shouldBe` 1
    
    it "eliminates dead code" $ do
      let stages = [ECall () "id" [], ECall () "normalize" []]
      let pipeline = EPipe () (EString () "text") stages
      let (optimized, stats) = runOpt defaultOptConfig (eliminateDeadCode pipeline)
      deadCodeRemoved stats `shouldBeGreaterThan` 0
    
    it "inlines simple functions" $ do
      let expr = ECall () "id" [EString () "text"]
      let (optimized, stats) = runOpt defaultOptConfig (inlineExpr expr)
      functionsInlined stats `shouldBe` 1
  
  describe "Property tests" $ do
    it "parsing and pretty-printing should be inverse" $ property $ \text ->
      case parseExpr (T.pack text) of
        Right expr -> case parseExpr (prettyPrintExpr expr) of
          Right expr' -> expr `shouldBe` expr'
          Left _ -> expectationFailure "Failed to parse pretty-printed expression"
        Left _ -> return () -- Skip invalid inputs
    
    it "optimization should preserve semantics" $ property $ \_ ->
      -- This would require a more sophisticated semantic equivalence check
      pending

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

shouldBeGreaterThan :: (Ord a, Show a) => a -> a -> Expectation
shouldBeGreaterThan actual expected = actual `shouldSatisfy` (> expected)

-- Placeholder for pretty printing (would need to be implemented)
prettyPrintExpr :: Expr a -> T.Text
prettyPrintExpr (EString _ s) = "\"" <> s <> "\""
prettyPrintExpr (EVar _ name) = name
prettyPrintExpr (ERegex _ pattern) = "/" <> pattern <> "/"
prettyPrintExpr _ = "<complex_expr>"
