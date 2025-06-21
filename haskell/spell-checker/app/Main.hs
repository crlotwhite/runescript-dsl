{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import SpellChecker.Parser
import SpellChecker.TypeInfer
import SpellChecker.Optimizer
import SpellChecker.AST

-- | Command line options
data Options = Options
  { optInputFile :: String
  , optOutputFile :: Maybe String
  , optTypeCheck :: Bool
  , optOptimize :: Bool
  , optVerbose :: Bool
  , optShowTypes :: Bool
  } deriving (Show)

-- | Parse command line options
options :: Parser Options
options = Options
  <$> strArgument
      ( metavar "FILE"
     <> help "Input .spell file to check" )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file for results" ))
  <*> switch
      ( long "type-check"
     <> short 't'
     <> help "Perform type checking" )
  <*> switch
      ( long "optimize"
     <> short 'O'
     <> help "Apply optimizations" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output" )
  <*> switch
      ( long "show-types"
     <> short 's'
     <> help "Show inferred types" )

-- | Main program
main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper)
    ( fullDesc
   <> progDesc "RuneScript Spell static checker"
   <> header "spell-checker - static analysis tool for .spell files" )
  
  runChecker opts

-- | Run the checker with given options
runChecker :: Options -> IO ()
runChecker opts = do
  when (optVerbose opts) $
    putStrLn $ "Processing file: " ++ optInputFile opts
  
  -- Read input file
  content <- TIO.readFile (optInputFile opts)
  
  -- Parse the file
  case parseSpellModule content of
    Left parseError -> do
      hPutStrLn stderr $ "Parse error: " ++ show parseError
      exitFailure
    
    Right parsedModule -> do
      when (optVerbose opts) $
        putStrLn $ "Successfully parsed " ++ show (length $ modFunctions parsedModule) ++ " functions"
      
      -- Type checking
      if optTypeCheck opts
        then do
          when (optVerbose opts) $
            putStrLn "Running type checker..."
          
          case inferModule parsedModule of
            Left typeError -> do
              hPutStrLn stderr $ "Type error: " ++ T.unpack (prettyTypeError typeError)
              exitFailure
            
            Right typedModule -> do
              when (optVerbose opts) $
                putStrLn "Type checking passed"
              
              when (optShowTypes opts) $ do
                putStrLn "\nInferred types:"
                mapM_ printFunctionType (modFunctions typedModule)
              
              -- Optimization
              if optOptimize opts
                then runOptimization opts typedModule
                else finishProcessing opts typedModule
        
        else do
          -- Skip type checking, proceed with untyped module
          if optOptimize opts
            then do
              when (optVerbose opts) $
                putStrLn "Running optimizations (without type checking)..."
              
              let (optimizedModule, stats) = optimizeModule defaultOptConfig parsedModule
              
              when (optVerbose opts) $
                printOptStats stats
              
              finishProcessing opts optimizedModule
            
            else finishProcessing opts parsedModule

-- | Run optimization phase
runOptimization :: Options -> Module Type -> IO ()
runOptimization opts typedModule = do
  when (optVerbose opts) $
    putStrLn "Running optimizations..."
  
  let (optimizedModule, stats) = optimizeModule defaultOptConfig typedModule
  
  when (optVerbose opts) $
    printOptStats stats
  
  finishProcessing opts optimizedModule

-- | Print optimization statistics
printOptStats :: OptStats -> IO ()
printOptStats stats = do
  putStrLn $ "Optimization results:"
  putStrLn $ "  Constants folded: " ++ show (constantsFolded stats)
  putStrLn $ "  Dead code removed: " ++ show (deadCodeRemoved stats)
  putStrLn $ "  Functions inlined: " ++ show (functionsInlined stats)

-- | Print function type information
printFunctionType :: FunDef Type -> IO ()
printFunctionType (FunDef name params _ annotation) = do
  let paramStr = T.intercalate ", " params
  putStrLn $ T.unpack $ name <> "(" <> paramStr <> ") :: " <> prettyType annotation

-- | Finish processing and output results
finishProcessing :: Show a => Options -> Module a -> IO ()
finishProcessing opts finalModule = do
  let result = "Processed module with " ++ show (length $ modFunctions finalModule) ++ " functions\n"
  
  case optOutputFile opts of
    Nothing -> putStr result
    Just outputFile -> do
      writeFile outputFile result
      when (optVerbose opts) $
        putStrLn $ "Results written to: " ++ outputFile
  
  when (optVerbose opts) $
    putStrLn "Processing completed successfully"

-- | Example usage function
exampleUsage :: IO ()
exampleUsage = do
  putStrLn "Example usage:"
  putStrLn "  spell-checker example.spell --type-check --optimize --verbose"
  putStrLn "  spell-checker input.spell -o output.txt --show-types"
  putStrLn "  spell-checker module.spell -tOv"
