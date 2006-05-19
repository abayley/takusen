
|
Module      :  Database.PostgreSQL.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> {- # OPTIONS -ddump-hi #-}

> module Database.PostgreSQL.Test.Enumerator (runTest) where

> import qualified Database.PostgreSQL.Test.PGFunctions as Low
> import Database.PostgreSQL.Enumerator
> import Database.Test.Performance as Perf
> import Database.Test.Enumerator
> import Control.Monad (when)
> import Control.Exception (throwDyn)
> import Test.MiniUnit
> import Database.Enumerator
> import Data.Int
> import System.Time


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   putStrLn "PostgreSQL tests"
>   let (user:pswd:dbname:_) = args
>   Low.runTest user
>   flip catchDB basicDBExceptionReporter $ withSession (connect [CAuser user]) (testBody runPerf)

> testBody runPerf = do
>   runFixture PGSqlFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction RepeatableRead
>   runTestTT "PostgreSQL performance tests" (map (flip catchDB reportRethrow)
>     [ timedSelect (prefetch 1000 sqlRows2Power20 []) 40 (2^20)
>     , timedSelect (prefetch 1 sqlRows2Power17 []) 40 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 5 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 40 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 5 (2^17)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


> runFixture :: DBLiteralValue a => a -> DBM mark Session ()
> runFixture fns = do
>   makeFixture execDrop execDDL_
>   runTestTT "Postgres tests" (map (runOneTest fns) testList)
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

> selectBoundaryDates dateFn = selectTest (sqlBoundaryDates dateFn) iterBoundaryDates expectBoundaryDates

> selectCursor fns = actionCursor (sqlCursor fns)

> selectExhaustCursor fns = actionExhaustCursor (sqlCursor fns)

Note that these two tests use the same prepared statement name.
This tests that the statement is properly deallocated, through use of
withPreparedStatement.

> selectBindString _ = actionBindString
>   (prepareStmt "1" (sql sqlBindString) [bindType "", bindType ""])
>   [bindP "a2", bindP "b1"]

> selectBindInt _ = actionBindInt
>   (prepareStmt "1" (sql sqlBindInt) (map bindType expectBindInt))
>   [bindP (1::Int), bindP (2::Int)]


> selectBindIntDoubleString _ = actionBindIntDoubleString
>   (prefetch 0 sqlBindIntDoubleString [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int), bindP (4.4::Double), bindP "row 2"])

> selectRebindStmt _ = actionRebind
>   (prepareStmt "1" (sql sqlRebind) [bindType (0::Int)])
>   [bindP (1::Int)] [bindP (2::Int)]

> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 0 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = actionPolymorphicFetchNull
>   (prefetch 1 sqlPolymorphicFetchNull [])

For the exceptionRollback test we have to specify the count result is int4;
if we don't specify the type then it defaults to a Postgres numeric,
which we can't yet marshal.

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4
>   ("select count(*)::int4 from " ++ testTable)

> testList :: DBLiteralValue a => [a -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   -- leave date-time for now... we don't know how to marshal it.
>   --, selectNullDate, selectDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   --, selectBindDate
>   , selectRebindStmt
>   , polymorphicFetchTest, polymorphicFetchTestNull
>   , exceptionRollback
>   ]

FIXME  Add tests for:
  dates
