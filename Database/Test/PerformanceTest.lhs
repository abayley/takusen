
|
Module      :  Database.Test.PerformanceTest
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Performance tests. Currently just tests large result sets.


> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Test.PerformanceTest (runPerformanceTest) where

> import Database.Oracle.Enumerator
> import System.Environment (getArgs)



2 ^ 16 = 65536
2 ^ 20 = 1048576

Cartesian product. Each instance of the subquery multiplies
the total number of rows by 2, so the number of rows
is 2 ^ (count of subquery instances).
You start to notice the pause around 2^13,
and 2^16 blows out the standard 1M stack
if you use the lazy version of result. Bummer.

> manyRows :: String
> manyRows = 
>   "select 1 from"
>   ++ "  ( select 1 from dual union select 0 from dual)"
>   ++ ", ( select 2 from dual union select 0 from dual)"
>   ++ ", ( select 3 from dual union select 0 from dual)"
>   ++ ", ( select 4 from dual union select 0 from dual)"
>   ++ ", ( select 5 from dual union select 0 from dual)"
>   ++ ", ( select 6 from dual union select 0 from dual)"
>   ++ ", ( select 7 from dual union select 0 from dual)"
>   ++ ", ( select 8 from dual union select 0 from dual)"
>   ++ ", ( select 9 from dual union select 0 from dual)"
>   ++ ", ( select 10 from dual union select 0 from dual)"
>   ++ ", ( select 11 from dual union select 0 from dual)"
>   ++ ", ( select 12 from dual union select 0 from dual)"
>   ++ ", ( select 13 from dual union select 0 from dual)"
>   ++ ", ( select 14 from dual union select 0 from dual)"
>   ++ ", ( select 15 from dual union select 0 from dual)"
>   ++ ", ( select 16 from dual union select 0 from dual)"
>   ++ ", ( select 17 from dual union select 0 from dual)"
>   ++ ", ( select 18 from dual union select 0 from dual)"
>   ++ ", ( select 19 from dual union select 0 from dual)"
>   ++ ", ( select 20 from dual union select 0 from dual)"



> rowCounter :: (Monad m) => Int -> IterAct m Int
> rowCounter _ i = result' (1 + i)  -- strict
> --rowCounter _ i = result (1 + i)  -- lazy


> selectLargeResultSet :: SessionQuery
> selectLargeResultSet = do
>   r <- doQuery manyRows rowCounter 0
>   liftIO $ putStrLn (show r)


> largeResultSet :: Session -> IO ()
> largeResultSet sess = catchDB ( do
>     putStrLn "\nlargeResultSet:"
>     runSession ( do
>         selectLargeResultSet
>       ) sess
>   ) basicDBExceptionReporter


> allTests :: Session -> IO ()
> allTests sess = do
>   largeResultSet sess


> runPerformanceTest :: IO ()
> runPerformanceTest = catchDB ( do
>     [ user, pswd, dbname ] <- getArgs
>     sess <- connect user pswd dbname
>     allTests sess
>     disconnect sess
>   ) basicDBExceptionReporter
