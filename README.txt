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



Paths and GHCi
--------------
Just as with ensuring that your path is correctly set when building Takusen,
you must also ensure it is correctly set when building your programs.
If it is not correct, then you are likely to see linker errors.

You can use Takusen with ghci by invoking ghci with the -l and -L options.
e.g.
  $ set libopts=
  $ set libopts=%libopts% -llibpq -L"C:\Program Files\PostgreSQL\8.1\bin"
  $ set libopts=%libopts% -lsqlite3 -L"C:\Program Files\sqlite3"
  $ set libopts=%libopts% -loci -L"C:\Program Files\Oracle\OraHome817\bin"
  $ ghci %libopts%

It is not possible to use the Takusen package in ghci if you are missing any
of the libraries i.e. you must have all of Sqlite, PostgreSQL, and Oracle
installed.
If you are missing one of these, then you will be able to start ghci and load
your Main module, but when you try to execute main it will attempt to link
against the missing library (even if you don't use it!).

The trick in this case is to work in the root folder of the Takusen
source tree i.e. to not use the installed package.

Note that this problem does not affect ghc (the compiler).
If you are missing a library,  but you don't use it, it will still
compile and link without errors.



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
