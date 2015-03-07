{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Language.Haskell.Interpreter (OptionVal ((:=)))
import           Language.Haskell.Interpreter hiding (Option, name)
import           System.Console.CmdArgs       as CA hiding ((:=))
import           System.Exit                  (exitFailure)
import           System.IO

import           Evaluator

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
  , "Data.List.Split" -- from split
  , "Data.Time"       -- from time
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
  &= program "hoe"
  &= summary "Haskell One-liner Evaluator"

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

  (_typ, _descr, f) <-
    choice [ (typ, descr, ) <$> compile (script opts)
           | (typ, descr, compile) <- evals
           ]
  liftIO $ hPutStrLn stderr $ "Use: " ++ _typ ++ " :: " ++ _descr
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
