{-# LANGUAGE QuasiQuotes #-}

module Evaluator (
  Script,
  Evaluator,
  evals
  ) where

import           Language.Haskell.Interpreter

import           Template

type Script = String -> IO String
type Evaluator = String -> Interpreter Script

evals :: [(String, Evaluator)]
evals =
  [ ( "String"
    , evaluator [t| \_ -> return bexpr |]
    )
  , ( "Show a"
    , evaluator [t| \_ -> return $ show expr ++ "\n" |]
    )
  , ( "IO ()"
    , evaluator [t| \_ -> do () <- expr; return "" |]
    )

  , ( "error"
    , evalErr
    )
  ]

evaluator :: (String -> String) -> Evaluator
evaluator templ expr = interpret (templ expr) (as :: Script)

evalErr :: Evaluator
evalErr expr = do
  typ <- typeOf expr
  fail $ "cannot evaluate type of: " ++ typ

-- evaluators

{-
evalIO :: Evaluator
evalIO s =
  evalCode $ "((" ++ s ++ ") >>= \\() -> return \"\")"

evalIOShow :: Evaluator
evalIOShow s =
  evalCode $ "return . show =<< (" ++ s ++ ")"

evalStrListToStrList :: Evaluator
evalStrListToStrList s =
  evalCode $ "unlines . (" ++ s ++ ") . lines"

evalStrListToStr :: Evaluator
evalStrListToStr s =
  evalCode $ "(++\"\\n\") . (" ++ s ++ ") . lines"

evalStrToStrList :: Evaluator
evalStrToStrList s =
  evalCode $ "unlines . (" ++ s ++ ")"

evalStrLineToStrLine :: Evaluator
evalStrLineToStrLine s =
  evalCode $ "unlines . map snd . sort . zipWith (" ++ s ++ ") [1..] . lines"

evalStrLineToStr :: Evaluator
evalStrLineToStr s =
  evalCode $ "unlines . zipWith (" ++ s ++ ") [1..] . lines"

evalStrToStr :: Evaluator
evalStrToStr s =
  evalCode $ "unlines . map (" ++ s ++ ") . lines"

evalCharToChar :: Evaluator
evalCharToChar s =
  evalCode $ "map (" ++ s ++ ")"
-}
