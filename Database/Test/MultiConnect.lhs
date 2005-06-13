
|
Module      :  Database.Test.MultiConnect
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Tests Database.Enumerator code in the context of multiple
database connections to different DBMS products.
We should add tests to shift data between databases, too,
but there are no tests for that (yet).


> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Test.MultiConnect (runTest) where

> import qualified Database.Sqlite.Enumerator as Sqlite (connect, disconnect)
> import qualified Database.Oracle.Enumerator as Oracle (connect, disconnect)
> import Database.Test.Enumerator as Enum
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import System.Environment (getArgs)

> runTest :: [String] -> IO ()
> runTest args = catchDB ( do
>     let [ user, pswd, dbname ] = args
>     sessOra <- Oracle.connect user pswd dbname
>     sessSql <- Sqlite.connect user pswd dbname
>     Enum.runTests dateSqlite sessSql
>     Enum.runTests dateOracle sessOra
>     Perf.runTests sessSql
>     Perf.runTests sessOra
>     Sqlite.disconnect sessSql
>     Oracle.disconnect sessOra
>   ) basicDBExceptionReporter
