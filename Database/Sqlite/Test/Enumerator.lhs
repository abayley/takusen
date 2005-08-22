
|
Module      :  Database.Sqlite.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Sqlite.Test.Enumerator (runTest) where

> import qualified Database.Sqlite.Test.SqliteFunctions as Low
> import Database.Sqlite.Enumerator (connect, disconnect)
> import Database.Test.Enumerator as Enum
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import Control.Monad (when)


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = catchDB ( do
>     let [_, _, dbname] = args
>     Low.runTest dbname
>     sess <- connect dbname 
>     Enum.runTests dateSqlite sess
>     when (runPerf == Perf.RunTests) (Perf.runTests sess)
>     disconnect sess
>   ) basicDBExceptionReporter
