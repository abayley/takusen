
|
Module      :  Database.Sqlite.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 

> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Sqlite.Test.Enumerator where

> import Database.Sqlite.Enumerator as Sqlite
> import Database.Test.Enumerator as Enum
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import System.Environment (getArgs)

> runTest :: IO ()
> runTest = catchDB ( do
>     sessSql <- logonSqlite
>     Enum.runTests dateSqlite sessSql
>     --Perf.runTests sessSql
>     Sqlite.disconnect sessSql
>   ) basicDBExceptionReporter

> logonSqlite = do
>   [ user, pswd, dbname ] <- getArgs
>   Sqlite.connect user pswd dbname
