{-# Language DeriveDataTypeable #-}

module Main (main) where

import Control.Monad hiding (join)
import Control.Monad.Error.Class
import System.Console.CmdArgs as CA
import System.IO

import Language.Haskell.Interpreter as HInt hiding (name)

data Option
  = Option
    { join :: Bool
    , inplace :: Maybe String
    , script :: String
    , inputFiles :: [String]
    , modules :: [String]
    }
  deriving (Show, Data, Typeable)

option =
  Option {
    join = def &= help "Join a type of script",
    inplace = def &= help "Edit files in place (make bkup if EXT supplied)" &= opt "" &= typ "EXT",
    script = def &= argPos 0 &= typ "SCRIPT", 
    inputFiles = def &= args &= typ "FILES",
    modules = def &= help "Import a module before running the script"
                  &= opt ""
                  &= explicit
                  &= name "mod"
                  &= name "m" }
  &= program "hoe"
  &= summary "Haskell One-liner Evaluator, (c) Hideyuki Tanaka 2010"

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
  setImportsQ $
    [ ("Prelude", Nothing)
    , ("Control.Applicative", Nothing)
    , ("Control.Monad", Nothing)
    , ("Data.Char", Nothing)
    , ("Data.List", Nothing)
    , ("Data.Ord", Nothing)
    , ("System.IO", Nothing)
    , ("System.IO.Unsafe", Nothing)
    , ("Text.Printf", Nothing)
    ] ++ [ (m, Nothing) | m <- modules opts ]
  set [ installedModulesInScope HInt.:= True ]
  
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
  
  let intr = genInteract opts
  
  choice (if join opts then reverse evals else evals) (script opts) intr

genInteract :: Main.Option -> (String -> String) -> IO ()
genInteract opts =
  case (inputFiles opts, inplace opts) of
    ([], _) -> interact
    (files, Nothing) -> \f -> do
      forM_ files $ \file -> do
        s <- readFile file
        putStr $ f s
    (files, Just ext) -> \f -> do
      forM_ files $ \file -> do
        s <- readFile file
        when (ext /= "") $ do
          writeFile (file ++ ext) s
        length s `seq` writeFile file (f s)

choice fs s intr = foldl1 (<|>) (map (\f -> f s intr) fs)

f <|> g = catchError f (\e -> g)

evalStr s _ = do
  r <- interpret s (as :: IO String)
  liftIO $ putStrLn =<< r
evalStrI s intr = do
  r <- interpret s (as :: String -> String)
  liftIO $ intr r

evalShow s =
  evalStr $ "return $ show (" ++ s ++ ")"
evalIO s =
  evalStr $ "((" ++ s ++ ") >>= \\() -> return \"\")"
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

evalErr s _ = do
  t <- typeOf s
  fail $ "cannot evaluate: " ++ t
