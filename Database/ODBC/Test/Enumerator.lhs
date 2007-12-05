
|
Module      :  Database.ODBC.Test.Enumerator
Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


> module Database.ODBC.Test.Enumerator (runTest) where

> import qualified Database.ODBC.Test.OdbcFunctions as Low
> import Database.ODBC.Enumerator
> import Database.Test.Performance as Perf
> import Database.Test.Enumerator
> import Database.Util
> import Control.Monad (when)
> import Control.Exception (throwDyn)
> import Test.MiniUnit


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   let (dsn:_) = args
>   Low.runTest args
>   putStrLn "ODBC Enum tests"
>   flip catchDB basicDBExceptionReporter $ do
>     (r, conn1) <- withContinuedSession (connect dsn) (testBody runPerf)
>     withSession conn1 testPartTwo

> testBody :: Perf.ShouldRunTests -> DBM mark Session ()
> testBody runPerf = do
>   runFixture ODBCFunctions
>   when (runPerf == Perf.RunTests) runPerformanceTests

> testPartTwo :: DBM mark Session ()
> testPartTwo = do
>   makeFixture execDrop execDDL_
>   destroyFixture execDDL_


> runPerformanceTests :: DBM mark Session ()
> runPerformanceTests = do
>   makeFixture execDrop execDDL_
>   beginTransaction RepeatableRead
>   runTestTT "ODBC performance tests" (map (flip catchDB reportRethrow)
>     [ timedSelect (prefetch 1000 sqlRows2Power20 []) 35 (2^20)
>     , timedSelect (prefetch 1 sqlRows2Power17 []) 4 (2^17)
>     , timedSelect (prefetch 1000 sqlRows2Power17 []) 4 (2^17)
>     , timedCursor (prefetch 1 sqlRows2Power17 []) 4 (2^17)
>     , timedCursor (prefetch 1000 sqlRows2Power17 []) 4 (2^17)
>     ]
>     )
>   commit
>   destroyFixture execDDL_


> runFixture :: DBLiteralValue a => a -> DBM mark Session ()
> runFixture fns = do
>   makeFixture execDrop execDDL_
>   runTestTT "ODBC tests" (map (runOneTest fns) testList)
>   destroyFixture execDDL_

> runOneTest fns t = catchDB (t fns) (reportRethrowMsg "runOneTest ")

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

> selectCursor fns = actionCursor (sqlCursor fns)
> selectExhaustCursor fns = actionExhaustCursor (sqlCursor fns)

> selectBindString _ = actionBindString
>     (prepareQuery (sql sqlBindString))
>     [bindP "a2", bindP "b1"]

> selectBindInt _ = actionBindInt
>   (prepareQuery (sql sqlBindInt))
>   [bindP (1::Int), bindP (2::Int)]


> selectBindIntDoubleString _ = actionBindIntDoubleString
>   (prefetch 1 sqlBindIntDoubleString [bindP (1::Int), bindP (2.2::Double), bindP "row 1", bindP (3::Int), bindP (4.4::Double), bindP "row 2"])

> selectBindDate _ = actionBindDate
>   (prefetch 1 sqlBindDate (map bindP expectBindDate))

> selectBindBoundaryDates _ = actionBindBoundaryDatesLocal
>   (prefetch 1 sqlBindBoundaryDates (map bindP expectBoundaryDatesLocal))

> expectBoundaryDatesLocal =
>   -- 1753 seems to be about the earliest year MS SQL Server supports.
>   [ int64ToUTCTime   17530101000000
>   , int64ToUTCTime   20010102000000
>   , int64ToUTCTime   20010103000000
>   , int64ToUTCTime   99991231000000
>   ]
> actionBindBoundaryDatesLocal stmt = do
>   withTransaction Serialisable $ do
>     actual <- doQuery stmt iterBindDate []
>     assertEqual sqlBindBoundaryDates expectBoundaryDatesLocal actual


> selectRebindStmt _ = actionRebind (prepareQuery (sql sqlRebind))
>    [bindP (1::Int)] [bindP (2::Int)]

> boundStmtDML _ = actionBoundStmtDML (prepareCommand (sql sqlBoundStmtDML))
> boundStmtDML2 _ = do
>   -- With MS SQL Server cannot use withTransaction with rollback/commit;
>   -- if you explicitly end the transaction, then when withTransaction
>   -- attempts to end it (with a commit, in the success case) then we
>   -- get a "logic error".
>   -- This differs from PostgreSQL and Oracle, who don't seem to care if
>   -- you commit or rollback too many times.
>   beginTransaction ReadCommitted
>   count <- execDML (cmdbind sqlBoundStmtDML [bindP (100::Int), bindP "100"])
>   rollback
>   assertEqual sqlBoundStmtDML 1 count

> polymorphicFetchTest _ = actionPolymorphicFetch
>   (prefetch 1 sqlPolymorphicFetch [bindP expectPolymorphicFetch])

> polymorphicFetchTestNull _ = actionPolymorphicFetchNull
>   (prefetch 1 sqlPolymorphicFetchNull [])

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4 sqlExceptionRollback


> testList :: DBLiteralValue a => [a -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   , selectCursor, selectExhaustCursor
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   , selectBindDate, selectBindBoundaryDates, selectRebindStmt
>   , boundStmtDML, boundStmtDML2
>   , polymorphicFetchTest, polymorphicFetchTestNull, exceptionRollback
>   ]
