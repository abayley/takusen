
|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple driver module, mainly for testing.
Import test modules and run test suites.
 
This project is hosted by Haskell-libs:
  <http://sf.net/projects/haskell-libs>
 
Source code for this project is at:
  <http://cvs.sf.net/viewcvs.py/haskell-libs/libs/takusen/>


> module Main where

> import Database.Sqlite.Test.SqliteFunctions as SqliteLow
> import Database.Sqlite.Test.Enumerator as Sqlite
> import Database.Sqlite.Test.Performance as SqlitePerf
> {-
> import Database.Oracle.Test.OCIFunctions as OracleLow
> import Database.Oracle.Test.Enumerator as Oracle
> import Database.Oracle.Test.Performance as OraclePerf
> -}
> import Database.Stub.Test.Enumerator as Stub



> main :: IO ()
> main = do
>   --testStub
>   testSqlite
>   --testOCI

> testStub = Stub.runTest

> testSqlite = do
>   SqliteLow.runTest
>   Sqlite.runTest
>   --SqlitePerf.runTest
> {-
> testOCI = do
>   OracleLow.runTest
>   Oracle.runTest
>   --OraclePerf.runTest
> -}
