{-# LANGUAGE QuasiQuotes #-}

module Evaluator (
  Script,
  Evaluator,
  evals
  ) where

import           Language.Haskell.Interpreter

import           Template                     (t)

type Script    = String -> IO String
type Evaluator = String -> Interpreter Script

types :: [(String, String, String, String)]
types =
  [ ("String", "string", "id", "id")
  , ("[String]", "lines", "lines", "unlines")
  , ("[[String]]", "table", "map words . lines", "unlines . map unwords")
  , ("a", "value", "read", "(++ \"\\n\") . show")
  , ("[a]", "value lines", "map read . lines", "unlines . map show")
  , ("[[a]]", "value table", "map (map read . words) . lines", "unlines . map (unwords . map show)")
  ]

evals :: [(String, String, Evaluator)]
evals =
  [ evaluator "IO ()" "execute action"
    [t| \_ -> do () <- expr; return "" |]
  , evaluator "Char -> Char" "map input string"
    [t| return . map expr |]
  ] ++
  [ evaluator outputType ("output " ++ outputDescr)
      (\expr -> "\\_ -> return . " ++ outputCode ++ " . " ++ expr)
  | (outputType, outputDescr, _, outputCode) <- reverse types
  ] ++
  [ evaluator ("IO " ++ outputType) ("output result" ++ outputDescr)
      (\expr -> "\\_ -> return . " ++ outputCode ++ " =<< " ++ expr)
  | (outputType, outputDescr, _, outputCode) <- reverse types
  ] ++
  [ evaluator
      (inputType ++ " -> " ++ outputType)
      ("transform " ++ inputDescr ++ " to " ++ outputDescr)
      (\expr -> "return . " ++ outputCode ++ " . " ++ expr ++ " . " ++ inputCode)
  | (inputType,  inputDescr,  inputCode, _)  <- types
  , (outputType, outputDescr, _, outputCode) <- reverse types
  ] ++
  [ ( "a", "error", evalErr ) ]

evaluator :: String -> String -> (String -> String) -> (String, String, Evaluator)
evaluator typ description templ =
  (typ, description, \expr -> interpret (templ $ "(" ++ expr ++ ")") (as :: Script))

evalErr :: Evaluator
evalErr expr = do
  typ <- typeOf expr
  fail $ "cannot evaluate type of: " ++ typ
