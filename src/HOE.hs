{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Data.Version                 (showVersion)
import           Language.Haskell.Interpreter (OptionVal ((:=)))
import           Language.Haskell.Interpreter hiding (Option, name)
import           System.Console.CmdArgs       as CA hiding ((:=))
import           System.Exit                  (exitFailure)
import           System.IO

import           Evaluator
import           Paths_hoe                    (version)

imports :: [String]
imports =
  [ "Prelude"

    -- from base
  , "Control.Applicative"
  , "Control.Arrow"
  , "Control.Monad"
  , "Data.Bits"
  , "Data.Char"
  , "Data.Complex"
  , "Data.Either"
  , "Data.Function"
  , "Data.List"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Numeric"
  , "System.IO"
  , "System.IO.Unsafe"
  , "System.Info"
  , "System.Random"
  , "Text.Printf"

    -- other common modules
  , "Data.List.Split"  -- from split
  , "Data.Time"        -- from time
  , "Text.Regex.Posix" -- from regex-posix
  ]

data Option
  = Option
    { inplace    :: Maybe String
    , script     :: String
    , inputFiles :: [String]
    , modules    :: [String]
    }
  deriving (Show, Data, Typeable)

option :: Option
option = Option
  { inplace =
      def &= help "Edit files in place (make bkup if EXT supplied)" &= opt "" &= typ "EXT"
  , script =
      def &= argPos 0 &= typ "SCRIPT"
  , inputFiles =
      def &= args &= typ "FILES"
  , modules =
      def &= help "Import a module before running the script"
          &= opt ""
          &= explicit
          &= name "mod"
          &= name "m"
  }
  &= verbosity
  &= program "hoe"
  &= summary ("hoe-" ++ showVersion version ++ " Haskell One-liner Evaluator, (c) Hideyuki Tanaka")
  &= details [ "The Awk like text processor, but it can use Haskell."
             , ""
             ]

printLog :: String -> IO ()
printLog msg = whenLoud $ hPutStrLn stderr msg

main :: IO ()
main = do
  opts <- cmdArgs option
  r <- evalOneLiner opts
  case r of
    Left err -> do
      case err of
        WontCompile errs ->
          hPutStrLn stderr $ "compile error: " ++ unlines (map errMsg errs)
        UnknownError msg ->
          hPutStrLn stderr msg
        _ ->
          hPrint stderr err
      exitFailure
    Right _ ->
      return ()

evalOneLiner :: Option -> IO (Either InterpreterError ())
evalOneLiner opts = runInterpreter $ do
  reset
  setImportsQ $
    [ (m, Nothing) | m <- imports ] ++
    [ (m, Nothing) | m <- modules opts ]
  set [ installedModulesInScope := True ]

  (ty, descr, f) <-
    choice [ (ty, descr, ) <$> compile (script opts)
           | (ty, descr, compile) <- evals
           ]

  liftIO $ printLog $ "Interpret as: " ++ ty ++ " :: " ++ descr
  liftIO $ exec opts f

choice :: [Interpreter a] -> Interpreter a
choice = foldl1 $ \a b -> catch a (\(_e :: SomeException) -> b)

exec :: Main.Option -> Script -> IO ()
exec opts f =
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
          writeFile (file ++ "." ++ ext) s
        length s `seq` writeFile file =<< f s
