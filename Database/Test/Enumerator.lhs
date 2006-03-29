
|
Module      :  Database.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple test harness. Demonstrates possible usage.
 
This shows that all you need to write generic database code
is Database.Enumerator.
You must to use a DBMS-specific library to create the Session
(and to disconnect), but everything else is generic.


> {-# OPTIONS -fglasgow-exts #-}

> module Database.Test.Enumerator
>   (runTests, dateSqlite, dateOracle, makeFixture, destroyFixture)
> where

> import Database.Enumerator
> import System.Time  -- CalendarTime
> import Data.Int
> --import Test.HUnit
> import Test.MiniUnit


> runTests :: (Typeable a, IE.ISession sess) => 
>	 (Int64 -> String) -> IE.ConnectA sess -> (forall mark. DBM mark sess a) -> IO a
> runTests dateFn = do
>   makeFixture
>   --runTestTT (TestList (makeTests (testList dateFn)))
>   runTestTT (testList dateFn)
>   destroyFixture

> testTable = "takusen_test"

> --makeTests list = map TestCase list


Oracle has a table called "dual" built-in, but other DBMS's don't.
We'll create one here called "tdual".

The dual table only contains one row, which is useful
in Oracle because all queries must have a from-clause.
If you want to select a literal value, you must say:
  "select value from dual"
With most (if not all) other DBMS's the from-clause is
optional e.g. you can just say "select value".

So if we know that a dual table will always be present
(and containing a single row) then we can use the same
sql statements for all DBMS's.

> --makeFixture :: (MonadSession m IO s q) => s -> IO ()
> withSession :: (Typeable a, IE.ISession sess) => 
>	 IE.ConnectA sess -> (forall mark. DBM mark sess a) -> IO a
> makeFixture = do
>   execDrop "drop table tdual"
>   execDDL_ "create table tdual (dummy varchar(1) primary key)"
>   beginTransaction Serialisable
>   execDDL_ "insert into tdual values ('X')"
>   commit
>   execDrop $ "drop table " ++ testTable
>   execDDL_ $ "create table " ++ testTable ++ " (id integer, v varchar(1000))"
>   beginTransaction Serialisable
>   execDDL_ $ "insert into " ++ testTable ++ " (id, v) values (1, '2')"
>   execDDL_ $ "insert into " ++ testTable ++ " (id, v) values (2, '2')"
>   execDDL_ $ "insert into " ++ testTable ++ " (id, v) values (3, '3')"
>   commit

> destroyFixture = do
>   execDDL_ "drop table tdual"
>   execDDL_ ("drop table " ++ testTable)


> execDDL_ sql = catchDB (execDDL sql) (liftIO . reportError sql)

Use this (execDrop) when it's likely to raise an error.

> execDrop sql = catchDB (execDDL sql) (\e -> return undefined)


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

> zeroPad :: Int -> Int64 -> String
> zeroPad n i =
>   if i < 0
>   then "-" ++ (zeroPad n (abs i))
>   else (take taken (repeat '0')) ++ (showi)
>   where
>     showi = show i
>     taken = if leni > n then 0 else n - leni
>     leni = length showi




testList has a particularly nasty type, which wouldn't be feasable
to copy into here.

> testList (dateFn::(Int64 -> String)) =
>   [ selectNoRows
> {-
>   , selectNoRows, selectTerminatesEarly, selectFloatsAndInts
>   , selectNullString, selectEmptyString, selectUnhandledNull, selectNullDate dateFn
>   , selectDate dateFn, selectBoundaryDates dateFn
>   , selectCursor
>   , selectExhaustCursor
>   , selectBindInt, selectBindIntDoubleString, selectBindDate
>   , polymorphicFetchTest
>   , updateRollback, preparedStatement
> -}
>   ]


> selectTest query iter expect = do
>   actual <- doQuery query iter []
>   assertEqual query expect actual

> selectNoRows = selectTest query iter expect
>   where
>     query = "select dummy from tdual where dummy = 'a' or dummy = '2' "
>     iter (c1::String) acc = result $ c1:acc
>     expect = []::[String]


> {-

This is the example of enough context being provided so that
no signature for the iteratee is necessary.
  iter :: (Monad m) => String -> IterAct m [String]

> selectTerminatesEarly sess = selectTest sess query iter expect
>   where
>     query = "select 'hello1' from tdual union select 'hello2' from tdual union select 'hello3' from tdual"
>     iter c1 acc = if c1 == "hello2"
>       then return (Left (c1:acc))
>       else result (c1:acc)
>     expect = ["hello2", "hello1"]


> selectFloatsAndInts sess = selectTest sess query iter expect
>   where
>     query = "select 4841.3403490431, -22340234 from tdual union select 33311.32332, 23789234 from tdual"
>     --iter :: (Monad m) => Double -> Int -> IterAct m [(Double, Int)]
>     iter (c1::Double) (c2::Int) acc = result $ (c1, c2):acc
>     expect = [ (33311.32332, 23789234) , (4841.3403490431, -22340234) ]


> selectNullString sess = selectTest sess query iter expect
>   where
>     query = "select 'hello1', 'hello2', null from tdual"
>     iter :: (Monad m) => String -> String -> Maybe String
>                          -> IterAct m [(String, String, Maybe String)]
>     iter c1 c2 c3 acc = result $ (c1, c2, c3):acc
>     expect = [ ("hello1", "hello2", Nothing) ]


> selectEmptyString sess = selectTest sess query iter expect
>   where
>     query = "select 'hello1', 'hello2', '' from tdual /* Oracle always fails this test */"
>     iter :: (Monad m) => String -> String -> Maybe String
>                          -> IterAct m [(String, String, Maybe String)]
>     iter c1 c2 c3 acc = result $ (c1, c2, c3):acc
>     expect = [ ("hello1", "hello2", Just "") ]



|Goal: test that a column which is not a Maybe throws
and exception when it receives a null.

> selectUnhandledNull sess = catchDB ( do
>       selectTest sess query iter expect
>       assertFailure query
>   ) (\e -> return () )
>   where
>     query = "select 'hello1', 'hello2', null from tdual"
>     iter :: (Monad m) => String -> String -> CalendarTime
>                          -> IterAct m [(String, String, CalendarTime)]
>     iter c1 c2 c3 acc = result $ (c1, c2, c3):acc
>     expect = []



> selectNullDate dateFn sess = selectTest sess query iter expect
>   where
>     query = "select 'hello1', 'hello2', " ++ (dateFn 0) ++ " from tdual"
>     nvl = makeCalTime 10101000000
>     iter :: (Monad m) => String -> String -> Maybe CalendarTime
>                          -> IterAct m [(String, String, CalendarTime)]
>     iter c1 c2 c3 acc = result $ (c1, c2, ifNull c3 nvl):acc
>     expect = [ ("hello1", "hello2", nvl) ]



> selectDate dateFn sess = selectTest sess query iter expect
>   where
>     query = "select " ++ (dateFn 20041225235959) ++ " from tdual"
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc
>     expect = [ (makeCalTime 20041225235959) ]


> selectBoundaryDates dateFn sess = selectTest sess query iter expect
>   where
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc
>     -- test Oracle date boundary cases
>     query =     "select  " ++ (dateFn   99991231000000)  ++ " from tdual"
>       ++ " union select  " ++ (dateFn      10101000000)  ++ " from tdual"
>       ++ " union select  " ++ (dateFn    (-10101000000)) ++ " from tdual"
>       ++ " union select  " ++ (dateFn (-47120101000000)) ++ " from tdual"
>       ++ " order by 1 desc"
>     expect =
>       [ makeCalTime (-47120101000000)
>       , makeCalTime    (-10101000000)
>       , makeCalTime      10101000000
>       , makeCalTime   99991231000000
>       ]


|Goal: exercise the  "happy path" throught cursor code
i.e. open and fetch all rows, close after last row.

> selectCursor sess = do
>   let
>     query = "select 1 from tdual union select 2 from tdual"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>   runSession sess $ withCursorTuned defaultResourceUsage query ([]) iter [] $ \c -> do
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

> selectExhaustCursor sess = do
>   let
>     query = "select 1 from tdual union select 2 from tdual"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>   catchDB (
>     runSession sess $ withCursorTuned defaultResourceUsage query ([]) iter [] $ \c -> do
>       cursorNext c
>       cursorNext c
>       cursorNext c
>       cursorNext c
>       liftIO $ assertFailure "selectExhaustCursor"
>     ) (\e -> return () )



> selectBindInt sess = do
>   let
>     -- Oracle only understands :x style placeholders;
>     -- we can use them as they are passed through unmolested.
>     -- ?-style seems to be ODBC only; ANSI SQL specifies :n, I believe.
>     --query = "select :x from tdual union select :x from tdual order by 1"
>     query = "select ? from tdual union select ? from tdual order by 1"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>     expect :: [Int]
>     expect = [2, 1]
>     bindVals = [dbBind (1::Int), dbBind (2::Int)]
>   actual <- runSession sess (doQueryTuned defaultResourceUsage query bindVals iter [])
>   assertEqual query expect actual

Demonstrates use of different types in bind positions.

> selectBindIntDoubleString sess = do
>   let
>     query = "select ?,?,? from tdual union select ?,?,? from tdual order by 1"
>     iter :: (Monad m) => Int -> Double -> String -> IterAct m [(Int, Double, String)]
>     iter i d s acc = result $ (i, d, s):acc
>     expect :: [(Int, Double, String)]
>     expect = [(3, 4.4, "row 2"), (1, 2.2, "row 1")]
>     bindVals = [dbBind (1::Int), dbBind (2.2::Double), dbBind "row 1", dbBind (3::Int), dbBind (4.4::Double), dbBind "row 2"]
>   actual <- runSession sess (doQueryTuned defaultResourceUsage query bindVals iter [])
>   assertEqual query expect actual

For Sqlite we use a 99999999999999 :: Int64 to represent a null date,
so this is sorted after non-null dates, which is the same behaviour as
SQL nulls. SQL nulls come last in the collation order.

> selectBindDate sess = do
>   let
>     query = "select ? from tdual union select ? from tdual order by 1"
>     iter :: (Monad m) => Maybe CalendarTime -> IterAct m [Maybe CalendarTime]
>     iter i acc = result $ i:acc
>     d1 = Just (makeCalTime 20050228093500)
>     d2 = Just (makeCalTime 20050227093500)
>     dnull :: Maybe CalendarTime; dnull = Nothing
>     expect :: [Maybe CalendarTime]; expect = [dnull, d2]
>     input = [dbBind dnull, dbBind d2]
>   actual <- runSession sess (doQueryTuned defaultResourceUsage query input iter [])
>   assertEqual query expect actual



> insertTable n s = do
>   executeDML ("insert into " ++ testTable ++ " (id, v) values (" ++ (show n) ++ ", '" ++ s ++ "')") []
>   return ()

> updateTable n s = do
>   executeDML ("update " ++ testTable ++ " set v = '" ++ s ++ "' where id = " ++ (show n)) []
>   return ()


> polymorphicFetchTest sess = runSession sess $ do
>   executeDML ("delete from " ++ testTable) []
>   let
>     l1 :: [Int]
>     l1 = [1, 2, 3, 4]
>     l2 :: [Int]
>     l2 = [5, 6, 7, 8]
>   beginTransaction Serialisable
>   insertTable 1 (show l1)
>   insertTable 2 (show l2)
>   commit
>   let
>     iter :: (Monad m) => [Int] -> IterAct m [[Int]]
>     iter c1 acc = result $ c1:acc
>     query = "select v from " ++ testTable ++ " order by id"
>     expect = [ l2, l1 ]
>     bindargs :: Monad ms => [stmt -> Position -> ms ()]
>     bindargs = []
>   actual <- doQueryTuned defaultResourceUsage query bindargs iter []
>   liftIO $ assertEqual query expect actual


> checkContents expect = do
>   let
>     iter (c1::Int) (c2::String) acc = result $ (c1, c2):acc
>     query = "select id, v from " ++ testTable ++ " order by id desc"
>   actual <- doQuery query iter []
>   liftIO $ assertEqual "checkContents" expect actual


> updateRollback sess = runSession sess $ do
>   executeDML ("delete from " ++ testTable) []
>   beginTransaction Serialisable
>   insertTable 1 "1"
>   insertTable 2 "2"
>   insertTable 3 "3"
>   commit
>   checkContents [ (1, "1"), (2, "2"), (3, "3") ]
>   beginTransaction Serialisable
>   updateTable 2 "22"
>   checkContents [ (1, "1"), (2, "22"), (3, "3") ]
>   rollback
>   checkContents [ (1, "1"), (2, "2"), (3, "3") ]
>   beginTransaction Serialisable
>   updateTable 2 "22"
>   commit
>   checkContents [ (1, "1"), (2, "22"), (3, "3") ]


> preparedStatement sess = runSession sess $ do
>   let sql = "update " ++ testTable ++ " set v = v where v = ?"
>   withStatement sql defaultResourceUsage $ \stmt -> do
>     bindParameters stmt [dbBind "xxx"]
>     rowcount <- executeStatement stmt
>     liftIO $ assertEqual "prepare" 0 rowcount
>     bindParameters stmt [dbBind "1"]
>     rowcount <- executeStatement stmt
>     liftIO $ assertEqual "prepare" 1 rowcount


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

> -}
