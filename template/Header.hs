{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language PackageImports #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import "mtl" Control.Monad.Trans

import Data.Bits
import Data.Char
import Data.Complex
import Data.Either
-- import Data.Foldable
-- import Data.Function
import Data.IORef
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.STRef
-- import Data.String
-- import Data.Traversable
import Data.Tuple
import Data.Unique
import Data.Version
import Data.Word

import System.CPUTime
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Info
import System.Mem
import System.Timeout

import Text.Printf

import Unsafe.Coerce

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Read as TL
