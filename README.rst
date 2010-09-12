hoe: Haskell One-liner Evaluator
================================

About This
----------

This program is Haskell one-liner evaluator.
It can evaluate a one-liner script in various ways depends on it's type.

Usage
-----

Hoe does line-wise behavior basically.
It accepts any function whose type is ([a] -> [a]) or (String -> String).

* Sort lines

 ::

  $ hoe 'sort'
  LICENSE
  README.rst
  README.rst~
  Setup.hs
  dist
  hoe.cabal
  src

* Drop lines

 ::
  $ ls | hoe 'drop 3'
  Setup.hs
  dist
  hoe.cabal
  src

* Add "> " to line head.

 ::
  $ ls | hoe '("> "++)'
  > LICENSE
  > README.rst
  > README.rst~
  > Setup.hs
  > dist
  > hoe.cabal
  > src

It also accepts (Char -> Char) functions.

* To uppercase

 ::
  $ ls | hoe 'toUpper'
  LICENSE
  README.RST
  README.RST~
  SETUP.HS
  DIST
  HOE.CABAL
  SRC

Of course, it can evaluate simple values.

* Calculate a integer value

 ::
  $ hoe '2^100'

* Make many files

 ::
  $ hoe 'forM [1..10] $ \i -> writeFile ("file."++show i) ""'
