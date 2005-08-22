
|
Module      :  Database.Test.Performance
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Performance tests. Currently just tests large result sets.


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> {-# OPTIONS -fno-monomorphism-restriction #-}

> module Database.Test.Performance (runTests, ShouldRunTests(..)) where

> import Database.Enumerator
> import qualified Database.Test.Enumerator as DBTest
> import System.Environment (getArgs)
> import Control.Monad
> import System.Time
> import Test.HUnit
> import Control.Monad.Reader

> data ShouldRunTests = RunTests | Don'tRunTests deriving (Show, Eq)

> runTests ::
>   ( MonadSession (ReaderT r IO) IO r
>   , MonadQuery m (ReaderT r IO) q b
>   , DBType Int m b
>   , QueryIteratee m (Int -> IterAct m Int) Int b
>   ) => r -> IO ()
> runTests sess = do
>   DBTest.makeFixture sess
>   runTestTT (TestList (makeTests sess testList))
>   DBTest.destroyFixture sess


> makeTests sess list = map (\f -> TestCase (f sess)) list

> testList =
>   [ selectLargeResultSet
>   , cursorLargeResultSetPrefetch
>   , cursorLargeResultSetNoPrefetch
>   ]



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
>   ++ "  ( select 1 from tdual union select 0 from tdual)"
>   ++ ", ( select 2 from tdual union select 0 from tdual)"
>   ++ ", ( select 3 from tdual union select 0 from tdual)"
>   ++ ", ( select 4 from tdual union select 0 from tdual)"
>   ++ ", ( select 5 from tdual union select 0 from tdual)"
>   ++ ", ( select 6 from tdual union select 0 from tdual)"
>   ++ ", ( select 7 from tdual union select 0 from tdual)"
>   ++ ", ( select 8 from tdual union select 0 from tdual)"
>   ++ ", ( select 9 from tdual union select 0 from tdual)"
>   ++ ", ( select 10 from tdual union select 0 from tdual)"
>   ++ ", ( select 11 from tdual union select 0 from tdual)"
>   ++ ", ( select 12 from tdual union select 0 from tdual)"
>   ++ ", ( select 13 from tdual union select 0 from tdual)"
>   ++ ", ( select 14 from tdual union select 0 from tdual)"
>   ++ ", ( select 15 from tdual union select 0 from tdual)"
>   ++ ", ( select 16 from tdual union select 0 from tdual)"
>   ++ ", ( select 17 from tdual union select 0 from tdual)"
>   ++ ", ( select 18 from tdual union select 0 from tdual)"
>   ++ ", ( select 19 from tdual union select 0 from tdual)"
>   ++ ", ( select 20 from tdual union select 0 from tdual)"



> rowCounter :: (Monad m) => Int -> IterAct m Int
> rowCounter _ i = result' (1 + i)  -- strict
> --rowCounter _ i = result (1 + i)  -- lazy


> prefetch1 = QueryResourceUsage 1
> prefetch1000 = QueryResourceUsage 1000

> selectLargeResultSet sess = runSession sess $ do
>   ct1 <- liftIO getClockTime
>   r <- doQueryTuned prefetch1000 manyRows [] rowCounter 0
>   let r = 1
>   ct2 <- liftIO getClockTime
>   let diffCt = diffClockTimes ct2 ct1
>   let secsDiff = TimeDiff 0 0 0 0 0 30 0
>   liftIO $ assertBool ("selectLargeResultSet: time " ++ (timeDiffToString diffCt)) (diffCt < secsDiff)
>   --liftIO $ assertEqual ("selectLargeResultSet: rows " ++ (show r)) 1048576 r
>   return ()


> cursorHelper msg resourceUsage secs = do
>   withCursorTuned resourceUsage manyRows [] rowCounter 0 $ \c -> do
>     ct1 <- liftIO getClockTime
>     replicateM_ 100000 (cursorNext c)
>     ct2 <- liftIO getClockTime
>     let diffCt = diffClockTimes ct2 ct1
>     let secsDiff = TimeDiff 0 0 0 0 0 secs 0
>     liftIO $ assertBool ("cursorLargeResultSet: time " ++ (timeDiffToString diffCt)) (diffCt < secsDiff)
>     --liftIO $ assertEqual ("cursorLargeResultSet: rows " ++ (show r)) 1048576 r

> cursorLargeResultSetNoPrefetch sess = runSession sess $ do
>   cursorHelper "no-prefetch" prefetch1 30

> cursorLargeResultSetPrefetch sess = runSession sess $ do
>   cursorHelper "prefetch-1000" prefetch1000 4
