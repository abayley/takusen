
|
Module      :  Database.Oracle.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Oracle.Test.Enumerator (runTest) where

> import qualified Database.Oracle.Test.OCIFunctions as Low
> import Database.Oracle.Enumerator
> import Database.Enumerator
> import Database.Test.Performance as Perf
> import Database.Test.Enumerator
> import Control.Monad (when)
> import Control.Exception (throwDyn)
> import Test.MiniUnit
> import Data.Int
> import System.Time


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   let (user:pswd:dbname:_) = args
>   Low.runTest args
>   flip catchDB basicDBExceptionReporter $ withSession (connect user pswd dbname) (testBody runPerf)

> testBody runPerf = do
>   runFixture OracleFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction RepeatableRead
>   runTestTT "Oracle performance tests" (map (flip catchDB reportRethrow)
>     [ timedSelect (prefetch 1000 sqlRows2Power20 []) 30 (2^20)
>     , timedSelect (prefetch 1 sqlRows2Power17 []) 60 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 6 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 60 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 6 (2^17)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


> runFixture :: DBLiteralValue a => a -> DBM mark Session ()
> runFixture fns = do
>   makeFixture execDrop execDDL_
>   runTestTT "Oracle tests" (map (runOneTest fns) testList)
>   destroyFixture execDDL_

> runOneTest fns t = catchDB (t fns) reportRethrow

-----------------------------------------------------------

> selectNoRows _ = selectTest sqlNoRows iterNoRows expectNoRows

> selectTerminatesEarly _ = selectTest sqlTermEarly iterTermEarly expectTermEarly

> selectFloatsAndInts fns = selectTest (sqlFloatsAndInts fns) iterFloatsAndInts expectFloatsAndInts

> selectNullString _ = selectTest sqlNullString iterNullString expectNullString

> selectEmptyString _ = selectTest sqlEmptyString iterEmptyString expectEmptyString

> selectUnhandledNull _ = catchDB ( do
>       selectTest sqlUnhandledNull iterUnhandledNull expectUnhandledNull
>       assertFailure sqlUnhandledNull
>   ) (\e -> return () )



> selectNullDate dateFn = selectTest (sqlNullDate dateFn) iterNullDate expectNullDate

> selectDate dateFn = selectTest (sqlDate dateFn) iterDate expectDate

> selectCalDate dateFn = selectTest (sqlDate dateFn) iterCalDate expectCalDate

> selectBoundaryDates dateFn = selectTest (sqlBoundaryDates dateFn) iterBoundaryDates expectBoundaryDates

> selectCursor fns = actionCursor (sqlCursor fns)

> selectExhaustCursor fns = actionExhaustCursor (sqlCursor fns)

> selectBindString _ = actionBindString
>     (prepareStmt (sql sqlBindString))
>     [bindP "a2", bindP "b1"]


> selectBindInt _ = actionBindInt
>   (prepareStmt (sql sqlBindInt))
>   [bindP (1::Int), bindP (2::Int)]


> selectBindIntDoubleString _ = actionBindIntDoubleString
>   (prefetch 0 sqlBindIntDoubleString [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int), bindP (4.4::Double), bindP "row 2"])

> selectBindDate _ = actionBindDate
>   (prefetch 1 sqlBindDate (map bindP expectBindDate))

> selectBindBoundaryDates _ = actionBindBoundaryDates
>   (prefetch 1 sqlBindBoundaryDates (map bindP expectBoundaryDates))

> selectRebindStmt _ = actionRebind (prepareStmt (sql sqlRebind))
>    [bindP (1::Int)] [bindP (2::Int)]

> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 0 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = actionPolymorphicFetchNull
>   (prefetch 1 sqlPolymorphicFetchNull [])

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4 sqlExceptionRollback

> testList :: DBLiteralValue a => [a -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   , selectNullDate, selectDate, selectCalDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   , selectBindDate, selectBindBoundaryDates, selectRebindStmt
>   , polymorphicFetchTest, polymorphicFetchTestNull
>   , exceptionRollback
>   ]
