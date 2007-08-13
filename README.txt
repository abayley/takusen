Installing
----------
Prerequisites: GHC >= 6.6, Cabal >= 1.1.6, filepath

(it's possible to use Takusen with GHC-6.4 and Cabal-1.1.4;
see notes in separate section below)

To run or build Setup.hs you will need filepath installed.
This is only needed by Setup.hs, and is not required by Takusen itself.

Ensure that the database libraries for any particular database you plan
to use are in your path e.g. ensure that %ORA_HOME%\bin is in your path.
If the library is not in your path, then it won't be detected in the
configure step, and Takusen won't be able to use it.
You can fix this by adding it to your path and going through the
configure/build/install cycle again.

Typical build, after unzipping the distribution archive (Takusen-?.?.gz):
  $ runhaskell Setup.lhs configure
  $ runhaskell Setup.lhs build
  $ runhaskell Setup.lhs install

Typical build, using darcs to get latest code:
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
{-# OPTIONS -fallow-overlapping-instances #-}
module Main where
import Database.Sqlite.Enumerator
import Control.Monad.Trans (liftIO)
main = flip catchDB reportRethrow $
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

Note that the Cabal build script detects which back-ends are installed by
looking for certain executables in your path. The script modifies the
package description in the build stage so that it only builds libraries
for the back-ends it detects. If you install a new back-end, you will
need to re-run the installation process in order for Takusen to use it.



PostgreSQL gotchas on Windows
-----------------------------
The PostgreSQL client library is called libpq.dll on Windows, rather than
the more typical pq.dll. This is fine when using ghc to compile, as gnu ld
is able to figure out that this is the library to use when you pass it -lpq,
but ghci is not quite so slick, and it fails to load the library.

There is any easy workaround, which is to copy libpq.dll and rename it to
pq.dll. If you do this, then you should be able to use ghci with PostgreSQL
without problems. Don't forget to ensure that PostgreSQL's bin is in your path.

In the past I've had problems with older versions of PostgreSQL and
ghc-6.4.1. Specifically, the call to PQprepare would segfault.
This occured with C programs (i.e. no Haskell) compiled with the gcc
and ld that came with ghc-6.4.1. If ld (version 2.15.91) was replaced
with an older one (2.13.91) from MSYS then the test program worked.

With PostgreSQL 8.1.5.1 and ghc-6.6 (gcc 3.4.5 (mingw special)
and ld 2.15.94) this problem seems to have vanished.



Oracle gotchas on Windows
-------------------------
Some users have reported linker errors because their Oracle bin contains
hsbase.dll, which is an Oracle library related to Heterogenous Services.
This DLL overrides GHC's HSbase.dll, and therefore causes linker errors.

If you can control your Oracle client installation then either
 - don't choose Heterogenous Services when you install,
   or re-run the installer and remove it, or
 - rename hsbase.dll in Oracle bin.



GHC-6.4 and Takusen
-------------------
It is possible to use Takusen with GHC-6.4. We have tested with Cabal-1.1.4,
so you aren't required to upgrade to 1.1.6 (but it won't hurt, either).
If you use Cabal-1.1.4, then you should use the Setup-114.hs script.
If you use Cabal-1.1.6, then you should use the normal Setup.hs script.

You will need to install Data.Time, which is quite a chore on Windows
because of the dependencies. You will also need MSYS installed in order
to be able to run autoreconf, so get that out of the way first.

The summary for Windows is:

darcs get --partial http://www-users.cs.york.ac.uk/~ndm/filepath/
(or download and unzip the prepared distribution)
normal cabal configure, build, install

darcs get --partial http://www.cse.unsw.edu.au/~dons/code/fps
(or download and unzip the prepared distribution)
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

And finally, build Takusen with the normal cabal configure,
build, install.
