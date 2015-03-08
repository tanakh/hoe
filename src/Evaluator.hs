module Evaluator (
  Script,
  Evaluator,
  evals
  ) where

import           Language.Haskell.Interpreter

type Script    = String -> IO String
type Evaluator = String -> Interpreter Script

types :: Bool -> [(String, String, String, String)]
types isInput
  | isInput = ss ++ vs
  | otherwise = reverse ss ++ reverse vs
  where
    ss =
      [ ("String", "string", "id", "id")
      , ("[String]", "lines", "lines", "unlines")
      , ("[[String]]", "table", "map words . lines", "unlines . map unwords")
      ]
    vs =
      [ ("a", "value", "read", "(++ \"\\n\") . show")
      , ("[a]", "value lines", "map read . lines", "unlines . map show")
      , ("[[a]]", "value table", "map (map read . words) . lines", "unlines . map (unwords . map show)")
      ]

evals :: [(String, String, Evaluator)]
evals =
  [ evaluator "IO ()" "execute action"
      (\expr -> "\\_ -> " ++ expr ++ " >>= \\() -> return \"\"")
  , evaluator "Char -> Char" "map input string"
      (\expr -> "\\_ -> return . map . " ++ expr)
  ] ++
  [ evaluator outputType ("output " ++ outputDescr)
      (\expr -> "\\_ -> return . " ++ outputCode ++ " . " ++ expr)
  | (outputType, outputDescr, _, outputCode) <- types False
  ] ++
  [ evaluator ("IO " ++ outputType) ("output result" ++ outputDescr)
      (\expr -> "\\_ -> return . " ++ outputCode ++ " =<< " ++ expr)
  | (outputType, outputDescr, _, outputCode) <- types False
  ] ++
  [ evaluator
      (inputType ++ " -> " ++ outputType)
      ("transform " ++ inputDescr ++ " to " ++ outputDescr)
      (\expr -> "return . " ++ outputCode ++ " . " ++ expr ++ " . " ++ inputCode)
  | (inputType,  inputDescr,  inputCode, _)  <- types True
  , (outputType, outputDescr, _, outputCode) <- types False
  ] ++
  [ ( "a", "error", evalErr ) ]

evaluator :: String -> String -> (String -> String) -> (String, String, Evaluator)
evaluator typ description templ =
  (typ, description, \expr -> interpret (templ $ "(" ++ expr ++ ")") (as :: Script))

evalErr :: Evaluator
evalErr expr = do
  typ <- typeOf expr
  fail $ "cannot evaluate type of: " ++ typ
