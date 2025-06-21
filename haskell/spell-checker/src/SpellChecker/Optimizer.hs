{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SpellChecker.Optimizer where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Reader

import SpellChecker.AST

-- | Optimization configuration
data OptConfig = OptConfig
  { enableConstantFolding :: Bool
  , enableDeadCodeElimination :: Bool
  , enableInlining :: Bool
  , maxInlineDepth :: Int
  } deriving (Show)

-- | Default optimization configuration
defaultOptConfig :: OptConfig
defaultOptConfig = OptConfig
  { enableConstantFolding = True
  , enableDeadCodeElimination = True
  , enableInlining = True
  , maxInlineDepth = 3
  }

-- | Optimization statistics
data OptStats = OptStats
  { constantsFolded :: Int
  , deadCodeRemoved :: Int
  , functionsInlined :: Int
  } deriving (Show)

-- | Optimization monad
type OptM = StateT OptStats (Reader OptConfig)

-- | Run optimization
runOpt :: OptConfig -> OptM a -> (a, OptStats)
runOpt config action = runReader (runStateT action initialStats) config
  where
    initialStats = OptStats 0 0 0

-- | Increment optimization counter
incStat :: (OptStats -> OptStats) -> OptM ()
incStat f = modify f

-- | Main optimization entry point
optimizeModule :: OptConfig -> Module a -> (Module a, OptStats)
optimizeModule config mod = runOpt config $ optimizeModuleM mod

-- | Optimize module
optimizeModuleM :: Module a -> OptM (Module a)
optimizeModuleM (Module funDefs imports) = do
  optimizedFunDefs <- mapM optimizeFunDef funDefs
  return $ Module optimizedFunDefs imports

-- | Optimize function definition
optimizeFunDef :: FunDef a -> OptM (FunDef a)
optimizeFunDef (FunDef name params body annotation) = do
  optimizedBody <- optimizeExpr body
  return $ FunDef name params optimizedBody annotation

-- | Optimize expression
optimizeExpr :: Expr a -> OptM (Expr a)
optimizeExpr expr = do
  config <- ask
  
  -- Apply optimizations in order
  expr1 <- if enableConstantFolding config
           then constantFoldExpr expr
           else return expr
  
  expr2 <- if enableInlining config
           then inlineExpr expr1
           else return expr1
  
  expr3 <- if enableDeadCodeElimination config
           then eliminateDeadCode expr2
           else return expr2
  
  return expr3

-- | Constant folding optimization
constantFoldExpr :: Expr a -> OptM (Expr a)
constantFoldExpr = \case
  ECall a "concat" [EString _ s1, EString _ s2] -> do
    incStat $ \s -> s { constantsFolded = constantsFolded s + 1 }
    return $ EString a (s1 <> s2)
  
  ECall a "length" [EString _ s] -> do
    incStat $ \s -> s { constantsFolded = constantsFolded s + 1 }
    -- For demonstration, we'll represent numbers as strings
    return $ EString a (T.pack $ show $ T.length s)
  
  EPipe a lhs stages -> do
    optimizedLhs <- constantFoldExpr lhs
    optimizedStages <- mapM constantFoldExpr stages
    -- Try to fold the entire pipeline if it's all constants
    case (optimizedLhs, optimizedStages) of
      (EString _ s, [ECall _ "normalize" []]) -> do
        incStat $ \s -> s { constantsFolded = constantsFolded s + 1 }
        -- Apply normalization at compile time
        let normalized = T.toLower $ T.strip s
        return $ EString a normalized
      _ -> return $ EPipe a optimizedLhs optimizedStages
  
  ECall a func args -> do
    optimizedArgs <- mapM constantFoldExpr args
    return $ ECall a func optimizedArgs
  
  ELambda a params body -> do
    optimizedBody <- constantFoldExpr body
    return $ ELambda a params optimizedBody
  
  other -> return other

-- | Function inlining optimization
inlineExpr :: Expr a -> OptM (Expr a)
inlineExpr expr = do
  config <- ask
  if maxInlineDepth config > 0
    then inlineExprWithDepth (maxInlineDepth config) expr
    else return expr

-- | Inline with depth limit
inlineExprWithDepth :: Int -> Expr a -> OptM (Expr a)
inlineExprWithDepth depth expr
  | depth <= 0 = return expr
  | otherwise = case expr of
      ECall a "id" [arg] -> do
        -- Inline identity function
        incStat $ \s -> s { functionsInlined = functionsInlined s + 1 }
        inlineExprWithDepth (depth - 1) arg
      
      ECall a func args -> do
        optimizedArgs <- mapM (inlineExprWithDepth (depth - 1)) args
        return $ ECall a func optimizedArgs
      
      EPipe a lhs stages -> do
        optimizedLhs <- inlineExprWithDepth (depth - 1) lhs
        optimizedStages <- mapM (inlineExprWithDepth (depth - 1)) stages
        return $ EPipe a optimizedLhs optimizedStages
      
      ELambda a params body -> do
        optimizedBody <- inlineExprWithDepth (depth - 1) body
        return $ ELambda a params optimizedBody
      
      other -> return other

-- | Dead code elimination
eliminateDeadCode :: Expr a -> OptM (Expr a)
eliminateDeadCode = \case
  -- Remove unused lambda parameters (simplified)
  ELambda a params body -> do
    optimizedBody <- eliminateDeadCode body
    -- For now, just return the lambda as-is
    -- A more sophisticated implementation would analyze variable usage
    return $ ELambda a params optimizedBody
  
  ECall a func args -> do
    optimizedArgs <- mapM eliminateDeadCode args
    return $ ECall a func optimizedArgs
  
  EPipe a lhs stages -> do
    optimizedLhs <- eliminateDeadCode lhs
    optimizedStages <- mapM eliminateDeadCode stages
    
    -- Remove no-op stages
    let filteredStages = filter (not . isNoOp) optimizedStages
    if length filteredStages < length optimizedStages
      then do
        incStat $ \s -> s { deadCodeRemoved = deadCodeRemoved s + (length optimizedStages - length filteredStages) }
        return $ case filteredStages of
          [] -> optimizedLhs
          _ -> EPipe a optimizedLhs filteredStages
      else return $ EPipe a optimizedLhs optimizedStages
  
  other -> return other
  where
    isNoOp :: Expr a -> Bool
    isNoOp (ECall _ "id" []) = True
    isNoOp (ECall _ "noop" []) = True
    isNoOp _ = False

-- | Pipeline optimization
optimizePipeline :: Expr a -> OptM (Expr a)
optimizePipeline (EPipe a lhs stages) = do
  -- Combine adjacent compatible operations
  optimizedStages <- combinePipelineStages stages
  return $ EPipe a lhs optimizedStages
optimizePipeline other = return other

-- | Combine adjacent pipeline stages
combinePipelineStages :: [Expr a] -> OptM [Expr a]
combinePipelineStages = \case
  [] -> return []
  [stage] -> return [stage]
  (stage1 : stage2 : rest) -> do
    combined <- tryCombinaStages stage1 stage2
    case combined of
      Just combinedStage -> do
        incStat $ \s -> s { constantsFolded = constantsFolded s + 1 }
        combinePipelineStages (combinedStage : rest)
      Nothing -> do
        restOptimized <- combinePipelineStages (stage2 : rest)
        return (stage1 : restOptimized)

-- | Try to combine two pipeline stages
tryCombinaStages :: Expr a -> Expr a -> OptM (Maybe (Expr a))
tryCombinaStages stage1 stage2 = case (stage1, stage2) of
  -- Combine normalize operations
  (ECall a1 "normalize" [], ECall a2 "trim" []) -> 
    return $ Just $ ECall a1 "normalize_and_trim" []
  
  -- Combine string operations
  (ECall a1 "to_lower" [], ECall a2 "trim" []) -> 
    return $ Just $ ECall a1 "to_lower_and_trim" []
  
  -- Default: no combination possible
  _ -> return Nothing

-- | Analyze expression complexity
analyzaComplexity :: Expr a -> Int
analyzaComplexity = \case
  EVar _ _ -> 1
  EString _ _ -> 1
  ERegex _ _ -> 1
  ECall _ _ args -> 1 + sum (map analyzaComplexity args)
  EPipe _ lhs stages -> 1 + analyzaComplexity lhs + sum (map analyzaComplexity stages)
  ELambda _ _ body -> 2 + analyzaComplexity body

-- | Check if expression is pure (no side effects)
isPure :: Expr a -> Bool
isPure = \case
  EVar _ _ -> True
  EString _ _ -> True
  ERegex _ _ -> True
  ECall _ func args -> func `notElem` impureFunctions && all isPure args
  EPipe _ lhs stages -> isPure lhs && all isPure stages
  ELambda _ _ body -> isPure body
  where
    impureFunctions = ["output", "print", "read_file", "write_file"]

-- | Extract common subexpressions
extractCommonSubexpressions :: Module a -> Module a
extractCommonSubexpressions mod = mod -- Placeholder implementation

-- | Utility function to strip whitespace (for constant folding)
stripText :: Text -> Text
stripText = T.strip
