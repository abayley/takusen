
|
Module      :  Database.Sqlite.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 

> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Sqlite.Test.Enumerator (runTest) where

> import Database.Sqlite.Enumerator as Sqlite
> import Database.Test.Enumerator
> import System.Environment (getArgs)
> import System.Time  -- CalendarTime
> import Data.List (intersperse)

> runTest :: IO ()
> runTest = catchDB ( do
>     sess <- argLogon
>     runTests dateSqlite sess
>     --runTests dateOracle sess
>     Sqlite.disconnect sess
>   ) basicDBExceptionReporter

> argLogon :: IO Session
> argLogon = do
>   [ user, pswd, dbname ] <- getArgs
>   Sqlite.connect user pswd dbname
