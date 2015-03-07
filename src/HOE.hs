{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad
import           Control.Monad.Catch
import           System.Console.CmdArgs       as CA
import           System.IO

import           Language.Haskell.Interpreter as HInt hiding (Option, name)

data Option
  = Option
    { joinType   :: Bool
    , inplace    :: Maybe String
    , script     :: String
    , inputFiles :: [String]
    , modules    :: [String]
    }
  deriving (Show, Data, Typeable)

option :: Option
option =
  Option {
    joinType = def &= help "Join a type of script",
    inplace = def &= help "Edit files in place (make bkup if EXT supplied)" &= opt "" &= typ "EXT",
    script = def &= argPos 0 &= typ "SCRIPT",
    inputFiles = def &= args &= typ "FILES",
    modules = def &= help "Import a module before running the script"
                  &= opt ""
                  &= explicit
                  &= name "mod"
                  &= name "m" }
  &= program "hoe"
  &= summary "Haskell One-liner Evaluator"

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
          hPutStrLn stderr msg
        _ ->
          hPrint stderr err
    Right _ ->
      return ()

evalOneLiner :: Option -> IO (Either InterpreterError ())
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
    ] ++
    [ (m, Nothing) | m <- modules opts ]
  set [ installedModulesInScope HInt.:= True ]

  let intr = genInteract opts

  f <- choice (map ($ script opts) $ if joinType opts then reverse evals else evals)
  liftIO $ intr f

type Interact = (String -> IO String) -> IO ()

genInteract :: Main.Option -> Interact
genInteract opts f =
  case (inputFiles opts, inplace opts) of
    ([], _) -> do
        s <- getContents
        putStr =<< f s

    (files, Nothing) ->
      forM_ files $ \file -> do
        s <- readFile file
        putStr =<< f s

    (files, Just ext) ->
      forM_ files $ \file -> do
        s <- readFile file
        when (ext /= "") $
          writeFile (file ++ ext) s
        length s `seq` writeFile file =<< f s

choice :: [Interpreter a] -> Interpreter a
choice = foldl1 (<|>)

f <|> g = catch f (\(_e :: SomeException) -> g)

type Evaluator = String -> Interpreter (String -> IO String)

evals :: [Evaluator]
evals =
  [ evalShow
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

evalStr :: Evaluator
evalStr script = do
  v <- interpret script (as :: IO String)
  return $ \_input -> v

evalStrI :: Evaluator
evalStrI script = do
  f <- interpret script (as :: String -> String)
  return $ \input -> return $ f input

evalShow s =
  evalStr $ "return $ show (" ++ s ++ ")"
evalIO s =
  evalStr $ "((" ++ s ++ ") >>= \\() -> return \"\")"
evalIOShow s =
  evalStr $ "return . show =<< (" ++ s ++ ")"
evalStrListToStrList s =
  evalStrI $ "unlines . (" ++ s ++ ") . lines"
evalStrListToStr s =
  evalStrI $ "(++\"\\n\") . (" ++ s ++ ") . lines"
evalStrToStrList s =
  evalStrI $ "unlines . (" ++ s ++ ")"
evalStrLineToStrLine s =
  evalStrI $ "unlines . map snd . sort . zipWith (" ++ s ++ ") [1..] . lines"
evalStrLineToStr s =
  evalStrI $ "unlines . zipWith (" ++ s ++ ") [1..] . lines"
evalStrToStr s =
  evalStrI $ "unlines . map (" ++ s ++ ") . lines"
evalCharToChar s =
  evalStrI $ "map (" ++ s ++ ")"

evalErr :: Evaluator
evalErr script = do
  t <- typeOf script
  fail $ "cannot evaluate: " ++ t
