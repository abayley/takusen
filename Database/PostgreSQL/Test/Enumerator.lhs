
|
Module      :  Database.PostgreSQL.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.PostgreSQL.Test.Enumerator (runTest) where

> import qualified Database.PostgreSQL.Test.PGFunctions as Low
> import Database.PostgreSQL.Enumerator (connect, disconnect, ConnectAttr (..))
> -- import Database.Test.Enumerator as Enum
> import Database.PostgreSQL.Test.En1 as Enum
> import Database.Test.Performance as Perf
> import Database.Enumerator
> import Control.Monad (when)
 

> --runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = catchDB ( do
>     let [dbname] = args
>     -- Low.runTest dbname
>     sess <- connect dbname 
>     Enum.runTests undefined sess
>     --Enum.runTests datePG sess
>     -- when (runPerf == Perf.RunTests) (Perf.runTests sess)
>     disconnect sess
>   ) basicDBExceptionReporter
