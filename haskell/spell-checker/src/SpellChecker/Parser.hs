{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SpellChecker.Parser where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import SpellChecker.AST

type Parser = Parsec Void Text

-- | Parse a complete .spell module
parseSpellModule :: Text -> Either (ParseErrorBundle Text Void) (Module ())
parseSpellModule = parse spellModule "<input>"

-- | Lexer configuration
scn :: Parser () -- space consumer with newlines
scn = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

sc :: Parser () -- space consumer without newlines
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse identifiers
identifier :: Parser Text
identifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let name = T.pack (first : rest)
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name
  where
    keywords = ["def", "if", "then", "else", "let", "in", "import"]

-- | Parse string literals
stringLiteral :: Parser Text
stringLiteral = lexeme $ char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

-- | Parse regex literals  
regexLiteral :: Parser Text
regexLiteral = lexeme $ char '/' >> T.pack <$> manyTill anySingle (char '/')

-- | Parse a complete spell module
spellModule :: Parser (Module ())
spellModule = do
  scn
  imports <- many importStmt
  functions <- many functionDef
  eof
  return $ Module functions imports

-- | Parse import statements
importStmt :: Parser Text
importStmt = do
  symbol "import"
  moduleName <- stringLiteral
  scn
  return moduleName

-- | Parse function definitions
functionDef :: Parser (FunDef ())
functionDef = do
  symbol "def"
  name <- identifier
  symbol "("
  params <- sepBy identifier (symbol ",")
  symbol ")"
  symbol "="
  body <- expression
  scn
  return $ FunDef name params body ()

-- | Parse expressions
expression :: Parser (Expr ())
expression = pipeExpression

-- | Parse pipe expressions (highest precedence)
pipeExpression :: Parser (Expr ())
pipeExpression = do
  first <- applicationExpression
  rest <- many (symbol "|>" >> applicationExpression)
  return $ case rest of
    [] -> first
    _  -> EPipe () first rest

-- | Parse function applications
applicationExpression :: Parser (Expr ())
applicationExpression = do
  func <- primaryExpression
  case func of
    EVar _ name -> do
      args <- optional $ between (symbol "(") (symbol ")") (sepBy expression (symbol ","))
      case args of
        Nothing -> return func
        Just argList -> return $ ECall () name argList
    _ -> return func

-- | Parse primary expressions
primaryExpression :: Parser (Expr ())
primaryExpression = choice
  [ EString () <$> stringLiteral
  , ERegex () <$> regexLiteral
  , EVar () <$> identifier
  , between (symbol "(") (symbol ")") expression
  , lambdaExpression
  , conditionalExpression
  ]

-- | Parse lambda expressions
lambdaExpression :: Parser (Expr ())
lambdaExpression = do
  symbol "\\" <|> symbol "lambda"
  params <- some identifier
  symbol "->"
  body <- expression
  return $ ELambda () params body

-- | Parse conditional expressions
conditionalExpression :: Parser (Expr ())
conditionalExpression = do
  symbol "if"
  condition <- expression
  symbol "then"
  thenExpr <- expression
  symbol "else"
  elseExpr <- expression
  -- For now, we'll represent conditionals as function calls
  return $ ECall () "if" [condition, thenExpr, elseExpr]

-- | Parse type annotations (for future use)
typeAnnotation :: Parser Type
typeAnnotation = choice
  [ TText <$ symbol "String"
  , TRegex <$ symbol "Regex"
  , TPhoneme <$ symbol "Phoneme"
  , do symbol "List"
       symbol "["
       elemType <- typeAnnotation
       symbol "]"
       return $ TFun elemType elemType -- Simplified for now
  , TVar <$> identifier
  ]

-- | Parse a single expression from text (for testing)
parseExpr :: Text -> Either (ParseErrorBundle Text Void) (Expr ())
parseExpr = parse (scn >> expression <* eof) "<expr>"

-- | Parse a function definition from text (for testing)
parseFunDef :: Text -> Either (ParseErrorBundle Text Void) (FunDef ())
parseFunDef = parse (scn >> functionDef <* eof) "<fundef>"
