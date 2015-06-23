{-# LANGUAGE DataKinds #-}

module Main (main) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Version                 (showVersion)
import           Language.Haskell.Interpreter hiding (get)
import           Options.Declarative
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
    -> Cmd "hoe: Haskell One-liner Evaluator" ()
hoe inplace script files modules = do
    compiled <- liftIO $ runInterpreter $ do
        reset
        setImportsQ $
            [ (m, Nothing) | m <- imports ] ++
            [ (m, Nothing) | m <- words $ get modules ]
        set [ installedModulesInScope := True ]
        compile $ get script

    case compiled of
        Left (WontCompile errs) ->
            liftIO $ hPutStr stderr $ "compile error: " ++ unlines (map errMsg errs)
        Left (UnknownError msg) ->
            liftIO $ hPutStrLn stderr msg
        Left err ->
            liftIO $ hPrint stderr err

        Right (ty, descr, f) -> do
            logStr 1 $ "Interpret as: " ++ ty ++ " :: " ++ descr
            liftIO $ exec (get files) (get inplace) f

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
