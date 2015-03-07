{-# LANGUAGE QuasiQuotes #-}

module Evaluator (
  Script,
  Evaluator,
  evals
  ) where

import           Language.Haskell.Interpreter

import           Template

type Script    = String -> IO String
type Evaluator = String -> Interpreter Script

evals :: [(String, String, Evaluator)]
evals =
  [ -- simple value
    evaluator "String" "output string"
    [t| \_ -> return expr |]

  , evaluator "Show a => a" "output value"
    [t| \_ -> return $ show expr ++ "\n" |]

    -- IO action
  , evaluator "IO ()" "execute action"
    [t| \_ -> do () <- expr; return "" |]

  , evaluator "IO String" "output result string"
    [t| \_ -> expr |]

  , evaluator "Show a => IO a" "output result value"
    [t| \_ -> expr >>= return . (++ "\n") . show |]

    -- input whole string
  , evaluator "Char -> Char" "map input string"
    [t| return . map expr input |]

  , evaluator "String -> String" "transform whole input string"
    [t| return . expr |]

  , evaluator "String -> [String]" "transform whole input string to lines"
    [t| return . unlines . expr |]

    -- input lines
  , evaluator "[String] -> String" "transform lines to string"
    [t| return . (++ "\n") . expr . lines |]

  , evaluator "[String] -> [String]" "transform lines to line"
    [t| return . unlines . expr . lines |]

  , ( "a", "error", evalErr )
  ]

evaluator :: String -> String -> (String -> String) -> (String, String, Evaluator)
evaluator typ description templ =
  (typ, description, \expr -> interpret (templ expr) (as :: Script))

evalErr :: Evaluator
evalErr expr = do
  typ <- typeOf expr
  fail $ "cannot evaluate type of: " ++ typ
