
|
Module      :  Database.PostgreSQL.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}
> {- # OPTIONS -ddump-hi #-}

> module Database.PostgreSQL.Test.Enumerator (runTest) where

> import qualified Database.PostgreSQL.Test.PGFunctions as Low
> import Database.PostgreSQL.Enumerator
> import qualified Database.Test.Performance as Perf
> import Control.Monad (when)
> import Test.MiniUnit
> import Database.Enumerator
> import Data.Int
> import System.Time


> runTest :: Perf.ShouldRunTests -> [String] -> IO ()
> runTest runPerf args = do
>   let (user:pswd:dbname:_) = args
>   Low.runTest user
>   withSession (connect [CAuser user]) testBody

> testBody = do
>     catchDB (
>         runFixture datePG
>         --when (runPerf == Perf.RunTests) (Perf.runTests sess)
>       ) basicDBExceptionReporter

> runFixture :: (Int64 -> String) -> DBM mark Session ()
> runFixture dateFn = do
>   makeFixture
>   runTestTT (map ($ dateFn) testList)
>   destroyFixture


-----------------------------------------------------------
Common stuff

> testTable = "takusen_test"

> sqlDropDual = "drop table tdual"
> sqlCreateDual = "create table tdual (dummy varchar(1) primary key)"
> sqlInsertDual = "insert into tdual values ('X')"
> sqlDropTest = "drop table " ++ testTable
> sqlCreateTest = "create table " ++ testTable ++ " (id integer, v varchar(1000))"
> sqlInsertTest1 = "insert into " ++ testTable ++ " (id, v) values (1, '2')"
> sqlInsertTest2 = "insert into " ++ testTable ++ " (id, v) values (2, '2')"
> sqlInsertTest3 = "insert into " ++ testTable ++ " (id, v) values (3, '3')"


> reportError sql (DBFatal (ssc, sssc) e m) = do
>   putStrLn ("FATAL: " ++ ssc ++ sssc ++ " - " ++ m)
>   putStrLn ("  " ++ sql)
> reportError sql (DBError (ssc, sssc) e m) = do
>   putStrLn (ssc ++ sssc ++ " - " ++ m)
>   putStrLn ("  " ++ sql)
> reportError sql (DBUnexpectedNull r c) =
>   putStrLn $ "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
> reportError sql (DBNoData) = putStrLn "Fetch: no more data."


> dateSqlite :: Int64 -> String
> dateSqlite i = if i == 0 then "99999999999999" else show i

> dateOracle :: Int64 -> String
> dateOracle i
>   | i == 0 = "to_date(null)"
>   | i > 0  = "to_date('" ++ (zeroPad 14 i) ++ "', 'yyyymmddhh24miss')"
>   | i < 0  = "to_date('" ++ (zeroPad 14 i) ++ "', 'syyyymmddhh24miss')"

> datePG :: Int64 -> String
> datePG i =
>   let
>     year = ((abs i) `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>     suffix = if i < 0 then " BC'" else " AD'"
>   in
>   if i == 0 then "null::timestamp"
>   else "timestamp '" ++ zp 4 year ++ "-" ++ zp 2 month ++ "-" ++ zp 2 day
>     ++ " " ++ zp 2 hour ++ ":" ++ zp 2 minute ++ ":" ++ zp 2 second ++ suffix

> zp = zeroPad

> zeroPad :: Int -> Int64 -> String
> zeroPad n i =
>   if i < 0
>   then "-" ++ (zeroPad n (abs i))
>   else take (n - length (show i)) (repeat '0') ++ show i

> makeCalTime :: Int64 -> CalendarTime
> makeCalTime i =
>   let
>     year = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in CalendarTime
>     { ctYear = fromIntegral year
>     , ctMonth = toEnum (fromIntegral month - 1)
>     , ctDay = fromIntegral day
>     , ctHour = fromIntegral hour
>     , ctMin = fromIntegral minute
>     , ctSec = fromIntegral second
>     , ctPicosec = 0
>     , ctWDay = Sunday
>     , ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }


> execDDL_ s = catchDB (execDDL (sql s)) (liftIO . reportError s)

Use this (execDrop) when it's likely to raise an error.

> execDrop s = catchDB (liftIO (putStrLn ("execDrop: " ++ s)) >> execDDL (sql s)) (\e -> liftIO $ putStrLn "execDrop error")

> sqlNoRows = "select dummy from tdual where dummy = 'a' or dummy = '2' "
> iterNoRows (c1::String) acc = result $ c1:acc
> expectNoRows = []::[String]

> sqlTermEarly = "select 'hello1' from tdual union select 'hello2' from tdual union select 'hello3' from tdual"
> iterTermEarly c1 acc = if c1 == "hello2"
>       then return (Left (c1:acc))
>       else result (c1:acc)
> expectTermEarly = ["hello2", "hello1"]

> sqlFloatsAndInts = "select 4841.3403490431, -22340234 from tdual union select 33311.32332, 23789234 from tdual"
> --iterFloatsAndInts :: (Monad m) => Double -> Int -> IterAct m [(Double, Int)]
> iterFloatsAndInts (c1::Double) (c2::Int) acc = result $ (c1, c2):acc
> expectFloatsAndInts = [ (33311.32332, 23789234) , (4841.3403490431, -22340234) ]

> sqlNullString = "select 'hello1', 'hello2', null from tdual"
> iterNullString :: (Monad m) => String -> String -> Maybe String
>   -> IterAct m [(String, String, Maybe String)]
> iterNullString c1 c2 c3 acc = result $ (c1, c2, c3):acc
> expectNullString = [ ("hello1", "hello2", Nothing) ]

> sqlEmptyString = "select 'hello1', 'hello2', '' from tdual /* Oracle always fails this test */"
> iterEmptyString :: (Monad m) => String -> String -> Maybe String
>                          -> IterAct m [(String, String, Maybe String)]
> iterEmptyString c1 c2 c3 acc = result $ (c1, c2, c3):acc
> expectEmptyString = [ ("hello1", "hello2", Just "") ]

> sqlUnhandledNull = "select 'hello1', 'hello2', null from tdual"
> iterUnhandledNull :: (Monad m) => String -> String -> CalendarTime
>                          -> IterAct m [(String, String, CalendarTime)]
> iterUnhandledNull c1 c2 c3 acc = result $ (c1, c2, c3):acc
> expectUnhandledNull = []

> sqlNullDate dateFn = "select 'hello1', 'hello2', " ++ (dateFn 0) ++ " from tdual"
> iterNullDate :: (Monad m) => String -> String -> Maybe CalendarTime
>                          -> IterAct m [(String, String, CalendarTime)]
> iterNullDate c1 c2 c3 acc = result $ (c1, c2, ifNull c3 (makeCalTime 10101000000)):acc
> expectNullDate = [ ("hello1", "hello2", (makeCalTime 10101000000)) ]

> sqlDate dateFn = "select " ++ (dateFn 20041225235959) ++ " from tdual"
> iterDate :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
> iterDate c1 acc = result $ c1:acc
> expectDate = [ (makeCalTime 20041225235959) ]


test Oracle date boundary cases

> sqlBoundaryDates dateFn =
>             "select  " ++ (dateFn   99991231000000)  ++ " from tdual"
>   ++ " union select  " ++ (dateFn      10101000000)  ++ " from tdual"
>   ++ " union select  " ++ (dateFn    (-10101000000)) ++ " from tdual"
>   ++ " union select  " ++ (dateFn (-47120101000000)) ++ " from tdual"
>   ++ " order by 1 desc"
> iterBoundaryDates :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
> iterBoundaryDates c1 acc = result $ c1:acc
> expectBoundaryDates =
>   [ makeCalTime (-47120101000000)
>   , makeCalTime    (-10101000000)
>   , makeCalTime      10101000000
>   , makeCalTime   99991231000000
>   ]


-----------------------------------------------------------

> makeFixture :: DBM mark Session ()
> makeFixture = do
>   execDrop sqlDropDual
>   execDrop sqlDropTest
>   execDDL_ sqlCreateDual
>   beginTransaction Serialisable
>   execDDL_ sqlInsertDual
>   commit
>   execDDL_ sqlCreateTest
>   beginTransaction Serialisable
>   execDDL_ sqlInsertTest1
>   execDDL_ sqlInsertTest2
>   execDDL_ sqlInsertTest3
>   commit

> destroyFixture = do
>   execDDL_ sqlDropDual
>   execDDL_ sqlDropTest

> selectTest query iter expect = do
>   actual <- doQuery (sql query) iter []
>   assertEqual query expect actual


> selectNoRows _ = selectTest sqlNoRows iterNoRows expectNoRows

> selectTerminatesEarly _ = selectTest sqlTermEarly iterTermEarly expectTermEarly

> selectFloatsAndInts _ = selectTest sqlFloatsAndInts iterFloatsAndInts expectFloatsAndInts

> selectNullString _ = selectTest sqlNullString iterNullString expectNullString

> selectEmptyString _ = selectTest sqlEmptyString iterEmptyString expectEmptyString

> selectUnhandledNull _ = catchDB ( do
>       selectTest sqlUnhandledNull iterUnhandledNull expectUnhandledNull
>       assertFailure sqlUnhandledNull
>   ) (\e -> return () )

> selectNullDate dateFn = selectTest (sqlNullDate dateFn) iterNullDate expectNullDate

> selectDate dateFn = selectTest (sqlDate dateFn) iterDate expectDate

> selectBoundaryDates dateFn = selectTest (sqlBoundaryDates dateFn) iterBoundaryDates expectBoundaryDates

|Goal: exercise the  "happy path" throught cursor code
i.e. open and fetch all rows, close after last row.

> selectCursor _ = do
>   let
>     query = "select 1 from tdual union select 2 from tdual"
>     --queryStmt = sql query
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>   withCursor query iter [] $ \c -> do
>     r <- cursorCurrent c
>     liftIO $ assertEqual query [1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual query False doneBool
>     --
>     newc <- cursorNext c
>     r <- cursorCurrent c
>     liftIO $ assertEqual query [2, 1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual query False doneBool
>     --
>     newc <- cursorNext c
>     r <- cursorCurrent c
>     liftIO $ assertEqual query [2, 1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual query True doneBool
>     --
>     r <- cursorCurrent c
>     liftIO $ assertEqual query [2, 1] r
>     doneBool <- cursorIsEOF c
>     liftIO $ assertEqual query True doneBool
>     --
>     return ()


|Goal: ensure exception raised when too many rows
fetched from cursor.
 
This test will raise an exception, as it tries to
fetch too many rows from the cursor.
The exception handler is coded as if we expect the
exception i.e. it ignores it.
The main action should never finish, so there's
a failure assertion at the bottom, just in case
the exception is not raised.

> selectExhaustCursor _ = do
>   let
>     query = "select 1 from tdual union select 2 from tdual"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>   catchDB (
>     withCursor query iter [] $ \c -> do
>       cursorNext c
>       cursorNext c
>       cursorNext c
>       cursorNext c
>       liftIO $ assertFailure "selectExhaustCursor"
>     ) (\e -> return () )

> selectBindInt _ = do
>   let
>     -- Oracle only understands :x style placeholders;
>     -- we can use them as they are passed through unmolested.
>     -- ?-style seems to be ODBC only; ANSI SQL specifies :n, I believe.
>     -- Sqlite and MS Sql Server use ?-style.
>     -- Postgres has it's own style too: $n
>     -- We can support ?-style and convert occurences of ? to
>     -- the back-end-specific style where required.
>     --query = "select :x from tdual union select :x from tdual order by 1"
>     query = "select ? from tdual union select ? from tdual order by 1"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>     expect :: [Int]
>     expect = [2, 1]
>     bindVals = [dbBind (1::Int), dbBind (2::Int)]
>   actual <- runSession sess (doQueryTuned defaultResourceUsage query bindVals iter [])
>   assertEqual query expect actual


> testList :: [(Int64 -> String) -> DBM mark Session ()]
> testList =
>   [ selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull, selectNullDate
>   -- leave date-time for now... we don't know how to marshal it.
>   --, selectDate, selectBoundaryDates
>   , selectCursor, selectExhaustCursor
> {-
>   , selectBindInt, selectBindIntDoubleString, selectBindDate
>   , polymorphicFetchTest
>   , updateRollback, preparedStatement
> -}
>   ]
