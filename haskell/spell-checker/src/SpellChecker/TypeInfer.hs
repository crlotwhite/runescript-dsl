{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SpellChecker.TypeInfer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import SpellChecker.AST

-- | Type inference state
data InferState = InferState
  { nextId :: Int
  , constraints :: [Constraint]
  } deriving (Show)

-- | Type constraint
data Constraint = Constraint Type Type
  deriving (Show, Eq)

-- | Type substitution
type Subst = Map Text Type

-- | Type inference monad
type InferM = StateT InferState (ReaderT TypeEnv (Either TypeError))

-- | Initial state
initialState :: InferState
initialState = InferState 0 []

-- | Generate fresh type variable
freshTyVar :: InferM Type
freshTyVar = do
  s <- get
  put s { nextId = nextId s + 1 }
  return $ TVar ("t" <> T.pack (show $ nextId s))

-- | Add constraint
addConstraint :: Type -> Type -> InferM ()
addConstraint t1 t2 = modify $ \s -> s { constraints = Constraint t1 t2 : constraints s }

-- | Lookup variable in environment
lookupVar :: Text -> InferM Type
lookupVar name = do
  env <- ask
  case lookup name env of
    Just ty -> return ty
    Nothing -> throwError $ UnboundVariable name

-- | Extend environment
extendEnv :: Text -> Type -> InferM a -> InferM a
extendEnv name ty = local ((name, ty) :)

-- | Infer type of expression
inferExpr :: Expr () -> InferM Type
inferExpr expr = case expr of
  EVar _ name -> lookupVar name
  
  EString _ _ -> return TText
  
  ERegex _ _ -> return TRegex
  
  ECall _ func args -> do
    argTypes <- mapM inferExpr args
    resultType <- freshTyVar
    funcType <- lookupVar func `catchError` \_ -> do
      -- If function not in environment, create a fresh function type
      let paramTypes = argTypes
      return $ foldr TFun resultType paramTypes
    
    -- Create expected function type
    let expectedType = foldr TFun resultType argTypes
    addConstraint funcType expectedType
    return resultType
  
  EPipe _ lhs stages -> do
    lhsType <- inferExpr lhs
    foldM inferPipeStage lhsType stages
  
  ELambda _ params body -> do
    paramTypes <- mapM (const freshTyVar) params
    let paramBindings = zip params paramTypes
    bodyType <- foldr (uncurry extendEnv) (inferExpr body) paramBindings
    return $ foldr TFun bodyType paramTypes

-- | Infer type of pipe stage
inferPipeStage :: Type -> Expr () -> InferM Type
inferPipeStage inputType stage = case stage of
  ECall _ func args -> do
    argTypes <- mapM inferExpr args
    resultType <- freshTyVar
    funcType <- lookupVar func `catchError` \_ -> do
      -- Create function type that takes input type as first argument
      let allArgTypes = inputType : argTypes
      return $ foldr TFun resultType allArgTypes
    
    let expectedType = foldr TFun resultType (inputType : argTypes)
    addConstraint funcType expectedType
    return resultType
  
  _ -> do
    stageType <- inferExpr stage
    resultType <- freshTyVar
    addConstraint stageType (TFun inputType resultType)
    return resultType

-- | Infer type of function definition
inferFunDef :: FunDef () -> InferM (FunDef Type)
inferFunDef (FunDef name params body _) = do
  paramTypes <- mapM (const freshTyVar) params
  let paramBindings = zip params paramTypes
  bodyType <- foldr (uncurry extendEnv) (inferExpr body) paramBindings
  let funType = foldr TFun bodyType paramTypes
  return $ FunDef name params body funType

-- | Solve constraints using unification
solve :: [Constraint] -> Either TypeError Subst
solve = unify Map.empty
  where
    unify :: Subst -> [Constraint] -> Either TypeError Subst
    unify subst [] = return subst
    unify subst (Constraint t1 t2 : cs) = do
      subst' <- unifyTypes (apply subst t1) (apply subst t2)
      let newSubst = compose subst' subst
      unify newSubst (map (applyConstraint newSubst) cs)
    
    applyConstraint :: Subst -> Constraint -> Constraint
    applyConstraint subst (Constraint t1 t2) = Constraint (apply subst t1) (apply subst t2)

-- | Unify two types
unifyTypes :: Type -> Type -> Either TypeError Subst
unifyTypes t1 t2 = case (t1, t2) of
  (TVar a, TVar b) | a == b -> return Map.empty
  (TVar a, t) -> bindVar a t
  (t, TVar a) -> bindVar a t
  (TText, TText) -> return Map.empty
  (TRegex, TRegex) -> return Map.empty
  (TPhoneme, TPhoneme) -> return Map.empty
  (TLang l1, TLang l2) | l1 == l2 -> return Map.empty
  (TFun a1 b1, TFun a2 b2) -> do
    s1 <- unifyTypes a1 a2
    s2 <- unifyTypes (apply s1 b1) (apply s1 b2)
    return $ compose s2 s1
  _ -> throwError $ UnificationError t1 t2

-- | Bind type variable
bindVar :: Text -> Type -> Either TypeError Subst
bindVar a t
  | t == TVar a = return Map.empty
  | a `occurs` t = throwError $ UnificationError (TVar a) t
  | otherwise = return $ Map.singleton a t
  where
    occurs :: Text -> Type -> Bool
    occurs a (TVar b) = a == b
    occurs a (TFun t1 t2) = occurs a t1 || occurs a t2
    occurs a (TLang _) = False
    occurs _ _ = False

-- | Apply substitution to type
apply :: Subst -> Type -> Type
apply subst t = case t of
  TVar a -> Map.findWithDefault t a subst
  TFun t1 t2 -> TFun (apply subst t1) (apply subst t2)
  _ -> t

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | Get free type variables
ftv :: Type -> Set Text
ftv (TVar a) = Set.singleton a
ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2
ftv _ = Set.empty

-- | Main type inference function
inferType :: TypeEnv -> Expr () -> Either TypeError Type
inferType env expr = do
  (ty, state) <- runReaderT (runStateT (inferExpr expr) initialState) env
  subst <- solve (constraints state)
  return $ apply subst ty

-- | Infer types for module
inferModule :: Module () -> Either TypeError (Module Type)
inferModule (Module funDefs imports) = do
  -- Create initial environment with built-in functions
  let builtinEnv = 
        [ ("normalize", TFun TText TText)
        , ("tokenize", TFun TText (TFun TText (TVar "List")))
        , ("korean_g2p", TFun TText TPhoneme)
        , ("phonemize", TFun TText TPhoneme)
        , ("length", TFun (TVar "a") (TVar "Number"))
        ]
  
  -- Infer types for all function definitions
  (typedFunDefs, finalState) <- runReaderT (runStateT (mapM inferFunDef funDefs) initialState) builtinEnv
  
  -- Solve all constraints
  subst <- solve (constraints finalState)
  
  -- Apply substitution to all function types
  let finalTypedFunDefs = map (fmap (apply subst)) typedFunDefs
  
  return $ Module finalTypedFunDefs imports

-- | Pretty print type
prettyType :: Type -> Text
prettyType TText = "String"
prettyType TRegex = "Regex"
prettyType TPhoneme = "Phoneme"
prettyType (TLang lang) = "Lang(" <> lang <> ")"
prettyType (TVar var) = var
prettyType (TFun t1 t2) = prettyType t1 <> " -> " <> prettyType t2

-- | Pretty print type error
prettyTypeError :: TypeError -> Text
prettyTypeError (UnificationError t1 t2) = 
  "Cannot unify " <> prettyType t1 <> " with " <> prettyType t2
prettyTypeError (UnboundVariable var) = 
  "Unbound variable: " <> var
prettyTypeError (KindError msg) = 
  "Kind error: " <> msg
