
|
Module      :  Database.Oracle.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 

> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Oracle.Test.Enumerator (runTest) where

> import Database.Oracle.Enumerator as Oracle
> import Database.Test.Enumerator
> import System.Environment (getArgs)
> import System.Time  -- CalendarTime
> import Data.List (intersperse)

> runTest :: IO ()
> runTest = catchDB ( do
>     sess <- argLogon
>     runTests dateOracle sess
>     Oracle.disconnect sess
>   ) basicDBExceptionReporter

> argLogon :: IO Session
> argLogon = do
>   [ user, pswd, dbname ] <- getArgs
>   Oracle.connect user pswd dbname
