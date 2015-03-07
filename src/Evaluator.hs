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

evals :: [(String, Evaluator)]
evals =
  [ ( "String"
    , evaluator [t| \_ -> return expr |]
    )
  , ( "Show a => a"
    , evaluator [t| \_ -> return $ show expr ++ "\n" |]
    )
  , ( "IO ()"
    , evaluator [t| \_ -> do () <- expr; return "" |]
    )
  , ( "IO String"
    , evaluator [t| \_ -> expr |]
    )
  , ( "Show a => IO a"
    , evaluator [t| \_ -> expr >>= return . show |]
    )

  , ( "Char -> Char"
    , evaluator [t| return . map expr input |]
    )

  , ( "String -> String"
    , evaluator [t| return . expr |]
    )
  , ( "String -> [String]"
    , evaluator [t| return . unlines . expr |]
    )

  , ( "[String] -> [String]"
    , evaluator [t| return . unlines . expr . lines |]
    )
  , ( "[String] -> String"
    , evaluator [t| return . (++ "\n") . expr . lines |]
    )

  , ( "error", evalErr )
  ]

evaluator :: (String -> String) -> Evaluator
evaluator templ expr = interpret (templ expr) (as :: Script)

evalErr :: Evaluator
evalErr expr = do
  typ <- typeOf expr
  fail $ "cannot evaluate type of: " ++ typ
