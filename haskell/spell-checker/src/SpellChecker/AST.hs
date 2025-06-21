{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module SpellChecker.AST where

import Data.Text (Text)

-- | Type system for RuneScript expressions
data Type
  = TText
  | TRegex  
  | TPhoneme
  | TLang Text  -- Language-specific type
  | TFun Type Type
  | TVar Text
  deriving (Show, Eq, Ord)

-- | Expression AST for .spell files
data Expr a
  = EVar a Text
  | EString a Text
  | ERegex a Text
  | ECall a Text [Expr a]
  | EPipe a (Expr a) [Expr a]
  | ELambda a [Text] (Expr a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Function definition
data FunDef a = FunDef
  { funName :: Text
  , funParams :: [Text]
  , funBody :: Expr a
  , funAnnot :: a
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Module definition
data Module a = Module
  { modFunctions :: [FunDef a]
  , modImports :: [Text]
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Type environment
type TypeEnv = [(Text, Type)]

-- | Type inference error
data TypeError
  = UnificationError Type Type
  | UnboundVariable Text
  | KindError Text
  deriving (Show, Eq)
