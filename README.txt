Installing
----------
Prerequisites: GHC >= 6.4, darcs, Cabal >= 1.1.6.1

At present we use darcs and Cabal to download, build, and install Takusen
(we plan to provide a zipped archive which should eliminate darcs from
the dependencies).
You will need ghc and darcs installed and on your path,
and you will need to upgrade Cabal to 1.1.6.1 (or better).
Note that Cabal-1.1.6, which is bundled with GHC-6.6, will NOT build Takusen.

To upgrade Cabal, download from http://www.haskell.org/cabal and unpack.
  $ runhaskell Setup.lhs configure
  $ runhaskell Setup.lhs build
  $ runhaskell Setup.lhs install

Ensure that the database libraries for any particular database you plan
to use are in your path e.g. ensure that %ORA_HOME%\bin is in your path.
If the library is not in your path, then it won't be detected in the
configure step, and Takusen won't be able to use it.
You can fix this by adding it to your path and going through the
configure/build/install cycle again.

Download Takusen:
  $ mkdir takusen
  $ cd takusen
  $ darcs get http://darcs.haskell.org/takusen
  $ runhaskell Setup.lhs configure
  $ runhaskell Setup.lhs build
  $ runhaskell Setup.lhs install
  

Using i.e. Writing data access code
-----------------------------------
There are extensive instructions and examples in the Haddock docs
for module Database.Enumerator:
 http://darcs.haskell.org/takusen/doc/html/Database-Enumerator.html

This should give you most, if not all, of the information you need to
create a program that uses Takusen.

Here's a little hello-world test case that uses Sqlite:

{-# OPTIONS -fglasgow-exts #-}
module Main where
import Database.Sqlite.Enumerator
import Database.Enumerator
import Control.Monad.Trans (liftIO)
main = do
  withSession (connect "sqlite_db") (do
    let iter (s::String) (_::String) = result s
    result <- doQuery (sql "select 'Hello world.'") iter ""
    liftIO (putStrLn result)
    )

If this is Main.hs, then
  $ ghc --make Main -o hello
should build a "hello" executable.



Paths, GHCi & runhaskell
------------------------
Just as with ensuring that your path is correctly set when building Takusen,
you must also ensure it is correctly set when building your programs.
If it is not correct, then you are likely to see linker errors.

It seems that it is not possible to use the Takusen package in ghci.
This appears to be because it tries to link all of the functions
in the package, rather than just the parts you're using.

This problem also affects runhaskell, unfortunately.
I think the solution might well be to split up the various
database-specific modules into separate packages, which is what
HSQL does.

Note that this problem does not affect ghc (the compiler).
If you are missing a library,  but you don't use it, it will still
compile and link without errors.

If you must use ghci, then one trick is to work in the root folder of the
Takusen source tree i.e. to not use the installed package.
If you recompile every module with ghc (perhaps do a ghc --make on your
main module) then that should save recompilation time, as ghci will use
precompiled .o files.




GHC-6.4 and Takusen
-------------------
If you must use Takusen with GHC-6.4, then you will need to install
Data.Time, which is quite a chore on Windows because of the dependencies.
You will need MSYS installed in order to run autoreconf,
so get that out of the way first.

The summary for Windows is:

darcs get --partial http://www.cse.unsw.edu.au/~dons/code/fps
normal cabal configure, build, install

darcs get http://darcs.haskell.org/packages/Win32
Edit Win32.cabal, add fps to build-depends.
normal cabal configure, build, install

darcs get http://semantic.org/TimeLib/TimeLib
There are two packages here: time and fixed. Ignore time.
Go into fixed and do cabal configure, build, install

darcs get http://darcs.haskell.org/packages/time
Edit time.cabal, add fixed and Win32 to build-depends.
create Setup.hs:
  import Distribution.Simple
  main = defaultMainWithHooks defaultUserHooks
From MSYS shell, not Windows cmd.exe:
  autoreconf
  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  runhaskell Setup.hs install
