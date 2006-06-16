
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
>   execDDL_ makeFixtureMultiResultSet1
>   runTestTT "Postgres tests" (map (runOneTest fns) testList)
>   execDDL_ dropFixtureMultiResultSet1
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

> polymorphicFetchTestNull _ = withTransaction RepeatableRead $ 
>   actionPolymorphicFetchNull (prefetch 1 sqlPolymorphicFetchNull [])

For the exceptionRollback test we have to specify the count result is int4;
if we don't specify the type then it defaults to a Postgres numeric,
which we can't yet marshal.

> exceptionRollback _ = actionExceptionRollback sqlInsertTest4
>   ("select count(*)::int4 from " ++ testTable)


> dropFixtureMultiResultSet1 = "DROP FUNCTION takusenTestFunc()"
> makeFixtureMultiResultSet1 =
>   "CREATE FUNCTION takusenTestFunc() RETURNS SETOF refcursor AS $$\n"
>   ++ "DECLARE\n"
>   ++ "    refc1 refcursor;\n"
>   ++ "    refc2 refcursor;\n"
>   ++ "BEGIN\n"
>   ++ "    OPEN refc1 FOR SELECT datname, datistemplate::int4, datallowconn::int4"
>   ++ " FROM pg_database order by datname;\n"
>   ++ "    RETURN NEXT refc1;\n"
>   ++ "    OPEN refc2 FOR SELECT proname, prorettype, prolang, prosrc FROM pg_proc;\n"
>   ++ "    RETURN NEXT refc2;\n"
>   ++ "END;\n"
>   ++ "$$ LANGUAGE plpgsql;\n"

> selectMultiResultSet _ = do
>   withTransaction RepeatableRead $ do
>   withPreparedStatement (prepareStmt "1" (sql "select * from takusenTestFunc()") []) $ \pstmt -> do
>   withBoundStatement pstmt [] $ \bstmt -> do
>     dummy <- doQuery bstmt iterMain []
>     result1 <- doQuery (NextResultSet pstmt) iterRS1 []
>     assertEqual "selectMultiResultSet: RS1" ("postgres", 0, 1) (head result1)
>     result2 <- doQuery (NextResultSet pstmt) iterRS2 []
>     assertBool "selectMultiResultSet: RS2" (length result2 > 1000)
>     return ()
>   where
>     iterMain :: (Monad m) => RefCursor -> IterAct m [RefCursor]
>     iterMain c acc = result (acc ++ [c])
>     iterRS1 :: (Monad m) => String -> Int -> Int -> IterAct m [(String, Int, Int)]
>     iterRS1 name templ conn acc = result (acc ++ [(name, templ, conn)])
>     iterRS2 :: (Monad m) => String -> Int -> Int -> String -> IterAct m [(String, Int, Int, String)]
>     iterRS2 name typ lang src acc = result (acc ++ [(name, typ, lang, src)])

Oracle:
let q = Proc "procname" [bindP "a", bindP RefCursor] [In, Out]
withPreparedStatement q $ \pstmt ->
withBoundStatement pstmt [] $ \bstmt ->
  dummy <- doQuery q iterMain
  where iterMain acc = return (Left ())

or:
let q = "select o.n, cursor(select n from naturals i where i.n < o.n) from naturals o where o.n < 10'
  iterMain (i::Int) (c::RefCursor) acc = result' ((i,c):acc)
  iterInner (i::Int) acc = result' (i:acc)
withPreparedStatement q $ \pstmt ->
withBoundStatement pstmt [] $ \bstmt ->
  rs1 <- doQuery q iterMain []
  let
    action (i,c) = do
      print i
      rs2 <- doQuery (NextResultSet q) iterInner []
      print rs2
      putStrLn ""
  mapM_ action rs1


> testList :: DBLiteralValue a => [a -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull
>   -- leave date-time for now... we don't know how to marshal it.
>   -- , selectNullDate, selectDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor
>   , selectBindString, selectBindInt, selectBindIntDoubleString
>   -- , selectBindDate
>   , selectRebindStmt
>   , polymorphicFetchTest, polymorphicFetchTestNull
>   , exceptionRollback
>   , selectMultiResultSet
>   ]

FIXME  Add tests for:
  dates
