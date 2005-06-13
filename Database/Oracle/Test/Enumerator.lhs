
|
Module      :  Database.Oracle.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Oracle.Test.Enumerator (runTest) where

> import qualified Database.Oracle.Test.OCIFunctions as Low
> import Database.Oracle.Enumerator
> import Database.Test.Enumerator as Enum
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import Control.Monad (when)


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = catchDB ( do
>     let [user, pswd, dbname ] = args
>     Low.runTest args
>     sess <- connect user pswd dbname
>     Enum.runTests dateOracle sess
>     when (runPerf == Perf.RunTests) (Perf.runTests sess)
>     disconnect sess
>   ) basicDBExceptionReporter
