{-# LANGUAGE RecordWildCards #-}
{-# Language DeriveDataTypeable #-}

import Control.Exception
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.Process

data Option =
  Option
  { script :: FilePath
  , options :: [String]
  }
  deriving (Data, Typeable, Show)

option :: Option
option = Option
  { script = def &= argPos 0 &= typ "SCRIPT"
  , options = def &= args &= typ "OPTION"
  }
  &= program "runhs"
  &= summary "Haskell Script, (c) Hideyuki Tanaka 2011"

main :: IO ()
main = do
  arg <- cmdArgs option
  exec arg

exec :: Option -> IO ()
exec Option {..} = do
  bracket
    (createTmp script)
    removeFile
    $ \scriptToRun -> do
      (_, _, _, ph) <- createProcess (shell $ "runhaskell " ++ scriptToRun ++ " " ++ unwords options)
      ec <- waitForProcess ph
      exitWith ec

createTmp :: FilePath -> IO FilePath
createTmp path = do
  header <- readFile "template/Header.hs"
  code <- readFile path
  writeFile outpath $ header ++ code
  return outpath
  where
    outpath = path ++ ".ext.hs"