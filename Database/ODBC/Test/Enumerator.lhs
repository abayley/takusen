
|
Module      :  Database.ODBC.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


 
> {- # OPTIONS -fglasgow-exts #-}
> {- # OPTIONS -fallow-overlapping-instances #-}

> module Database.ODBC.Test.Enumerator (runTest) where

> import qualified Database.ODBC.Test.OdbcFunctions as Low
> import Database.Test.Performance as Perf
> import Test.MiniUnit


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   --let (user:pswd:dbname:_) = args
>   Low.runTest args
