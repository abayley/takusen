
|
Module      :  Database.Oracle.Test.Performance
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Performance tests. Currently just tests large result sets.


> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Oracle.Test.Performance (runTest) where

> import Database.Oracle.Enumerator
> import System.Environment (getArgs)
> import Control.Monad
> import System.Time


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


> prefetch1 = QueryResourceUsage 1
> prefetch1000 = QueryResourceUsage 1000

> selectLargeResultSet :: SessionQuery
> selectLargeResultSet = do
>   ct1 <- liftIO getClockTime
>   r <- doQueryTuned prefetch1000 manyRows rowCounter 0
>   ct2 <- liftIO getClockTime
>   liftIO $ putStr "largeResultSet:"
>   liftIO $ putStrLn (show r)
>   liftIO $ putStr "selectLargeResultSet timing: "
>   liftIO $ putStrLn $ timeDiffToString $ diffClockTimes ct2 ct1

> cursorHelper :: String -> QueryResourceUsage -> SessionQuery
> cursorHelper msg resourceUsage = do
>   withCursorBracketTuned resourceUsage manyRows rowCounter 0 $ \c -> do
>     ct1 <- liftIO getClockTime
>     replicateM_ 100000 (do _ <- cursorNext c; return())
>     ct2 <- liftIO getClockTime
>     liftIO $ putStr $ "cursorLargeResultSet " ++ msg ++ " timing: "
>     liftIO $ putStrLn $ timeDiffToString $ diffClockTimes ct2 ct1

> cursorLargeResultSet :: SessionQuery
> cursorLargeResultSet = do
>   cursorHelper "no-prefetch" prefetch1
>   cursorHelper "prefetch-1000" prefetch1000



> largeResultSet :: Session -> IO ()
> largeResultSet sess = catchDB ( do
>     runSession ( do
>         selectLargeResultSet
>         cursorLargeResultSet
>       ) sess
>   ) basicDBExceptionReporter


> allTests :: Session -> IO ()
> allTests sess = do
>   largeResultSet sess


> runTest :: IO ()
> runTest = catchDB ( do
>     [ user, pswd, dbname ] <- getArgs
>     sess <- connect user pswd dbname
>     putStrLn "-- Performance Tests"
>     allTests sess
>     disconnect sess
>   ) basicDBExceptionReporter