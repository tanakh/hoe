{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Data.Version                 (showVersion)
import           Language.Haskell.Interpreter hiding (get)
import           Options.Declarative
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

main :: IO ()
main = run "hoe" (Just $ showVersion version) hoe

hoe :: Flag "i" '["inplace"] "EXT" "Edit files in-place (make backup if EXT is not null)" (Maybe String)
    -> Arg "SCRIPT" String
    -> Arg "[FILES]" [String]
    -> Flag "m" '["mod"] "MODULES" "Import modules before running the script" (Def "" String)
    -> Cmd "hoe: Haskell One-liner Evaluator"
hoe inplace script files modules = Cmd $ do
    result <- runInterpreter $ do
        reset
        setImportsQ $
            [ (m, Nothing) | m <- imports ] ++
            [ (m, Nothing) | m <- words $ get modules ]
        set [ installedModulesInScope := True ]

        (ty, descr, f) <-
            choice [ (ty, descr, ) <$> compile (get script)
                   | (ty, descr, compile) <- evals
                   ]

        liftIO $ printLog $ "Interpret as: " ++ ty ++ " :: " ++ descr
        liftIO $ exec (get files) (get inplace) f

    case result of
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

exec :: [String] -> Maybe String -> Script -> IO ()
exec [] _ f = putStr =<< f =<< getContents

exec files mbext f =
    forM_ files $ \file -> do
        s <- readFile file
        case mbext of
            Nothing -> putStr =<< f s
            Just ext -> do
                when (ext /= "") $ writeFile (file ++ "." ++ ext) s
                length s `seq ` writeFile file =<< f s

choice :: [Interpreter a] -> Interpreter a
choice = foldl1 $ \a b -> catch a (\(_e :: SomeException) -> b)

printLog :: String -> IO ()
printLog msg = {- whenLoud $ -} hPutStrLn stderr msg
