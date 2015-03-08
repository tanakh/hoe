hoe: Haskell One-liner Evaluator
================================

About
-----

`hoe` is AWK like text processor, but can use Haskell.
It can process text files in various ways depends on types of given scripts.

Usage
-----

To show basic usage, run `hoe --help`.

```
hoe Haskell One-liner Evaluator, (c) Hideyuki Tanaka

hoe [OPTIONS] SCRIPT [FILES]

Common flags:
  -i --inplace[=EXT]  Edit files in place (make backup if EXT supplied)
  -m --mod[=ITEM]     Import a module before running the script
  -? --help           Display help message
  -V --version        Print version information
  -v --verbose        Loud verbosity
  -q --quiet          Quiet verbosity
```

* Sort lines

```sh
$ ls | hoe 'lines >>> sort'
LICENSE
README.md
Setup.hs
cabal.sandbox.config
dist
hoe.cabal
src
template
test
```

* Drop lines

```bash
ls | hoe 'lines >>> drop 3'
cabal.sandbox.config
dist
hoe.cabal
src
template
test
```

* Add "> " to line head.

```bash
ls | hoe 'map ("> " ++)'
> LICENSE
> README.md
> Setup.hs
> cabal.sandbox.config
> dist
> hoe.cabal
> src
> template
> test
```

It also accepts `Char -> Char` functions.

* To uppercase

```
$ ls | hoe 'toUpper'
LICENSE
README.MD
SETUP.HS
CABAL.SANDBOX.CONFIG
DIST
HOE.CABAL
SRC
TEMPLATE
TEST
```

Of course, it can evaluate simple values.

* Calculate a integer value

```
$ hoe '2^100'
1267650600228229401496703205376
```

* Make many files

```
$ hoe 'forM [1..10] $ \i -> writeFile ("file."++show i) ""'
```
