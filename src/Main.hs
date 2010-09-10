{-# Language DeriveDataTypeable #-}

module Main (main) where

import Control.Monad.Error.Class
import System.Console.CmdArgs as CA
import System.IO

import Language.Haskell.Interpreter
import Language.Haskell.Exts

data Option
  = Option
    { join :: Bool
    , script :: String
    , inputFiles :: [String]
    }
  deriving (Show, Data, Typeable)

option =
  Option {
    join = def &= help "Join a type of script",
    script = def &= argPos 0 &= typ "SCRIPT", 
    inputFiles = def &= args &= typ "FILES" }
  &= program "hoe"
  &= summary "Haskell One-liner Envaliation, (c) Hideyuki Tanaka 2010"

main :: IO ()
main = do
  opts <- cmdArgs option
  r <- evalOneLiner opts
  case r of
    Left err ->
      case err of
        WontCompile errs ->
          hPutStrLn stderr $ "compile error: " ++ unlines (map errMsg errs)
        UnknownError msg ->
          hPutStrLn stderr $ msg
        _ ->
          hPutStrLn stderr $ show err
    Right _ ->
      return ()

evalOneLiner opts = runInterpreter $ do
  reset
  setImportsQ
    [ ("Prelude", Nothing)
    , ("Control.Applicative", Nothing)
    , ("Control.Monad", Nothing)
    , ("Data.Char", Nothing)
    , ("Data.List", Nothing)
    , ("Data.Ord", Nothing)
    , ("System.IO", Nothing)
    , ("System.IO.Unsafe", Nothing)
    , ("Text.Printf", Nothing)
    ]
  
  let evals
        = [ evalShow
          , evalIO
          , evalIOShow
          , evalStrListToStrList
          , evalStrListToStr
          , evalStrToStrList
          , evalStrLineToStrLine
          , evalStrLineToStr
          , evalStrToStr
          , evalCharToChar
          , evalErr
          ]
  
  choice (if join opts then reverse evals else evals) $ script opts

choice fs s = foldl1 (<|>) (map (\f -> f s) fs)

f <|> g = catchError f (\e -> g)

evalStr s = do
  r <- interpret s (as :: IO String)
  liftIO $ putStr =<< r
evalStrI s = do
  r <- interpret s (as :: String -> String)
  liftIO $ interact r

evalShow s =
  evalStr $ "return $ show (" ++ s ++ ")"
evalIO s =
  evalStr $ "((" ++ s ++ ") >> return \"\")"
evalIOShow s =
  evalStr $ "return . show =<< (" ++ s ++ ")"
evalStrListToStrList s =
  evalStrI $ "unlines . (" ++ s ++ ") . lines"
evalStrListToStr s = do
  evalStrI $ "(++\"\\n\") . (" ++ s ++ ") . lines"
evalStrToStrList s =
  evalStrI $ "unlines . (" ++ s ++ ")"
evalStrLineToStrLine s = do
  evalStrI $ "unlines . map snd . sort . zipWith (" ++ s ++ ") [1..] . lines"
evalStrLineToStr s = do
  evalStrI $ "unlines . zipWith (" ++ s ++ ") [1..] . lines"
evalStrToStr s = do
  evalStrI $ "unlines . map (" ++ s ++ ") . lines"
evalCharToChar s = do
  evalStrI $ "map (" ++ s ++ ")"

evalErr s = do
  t <- typeOf s
  fail $ "cannot evaluate: " ++ t
