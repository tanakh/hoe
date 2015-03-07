{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Template (t) where

import           Data.List.Split
import           Data.Monoid
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

t :: QuasiQuoter
t = QuasiQuoter {..} where
  quoteExp s = do
    name <- newName "expr"

    let f l r = [| $l <> "(" <> $(varE name) <> ")" <> $r |]
        ret = foldr1 f $ map stringE $ splitOn "expr" s

    [| \ $(varP name) -> $ret |]
