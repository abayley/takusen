
|
Module      :  Database.PostgreSQL.Test.PGFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 

> {-# OPTIONS -fglasgow-exts #-}

> module Database.PostgreSQL.Test.PGFunctions (runTest) where


> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Database.PostgreSQL.PGFunctions
> import System.Environment (getArgs)
> import Test.MiniUnit
> import Data.Time



> runTest :: String -> IO ()
> runTest dbname = printPropagateError $ do
>   testOpen ("user=" ++ dbname)
>   db <- openDb ("user=" ++ dbname)
>   disableNoticeReporting db
>   createFixture db
>   runTestTT "Postgres low-level tests" (testlist db)
>   destroyFixture db
>   closeDb db


> testlist db =
>   --TestList $ map (\t -> TestCase (t db))
>   map ($ db)
>   [ testSelectInts
>   , testSelectDouble
>   , testSelectDate
>   , testSelectDate2
>   , testSelectDate3
>   --, testSelectNumeric
>   , testSelectStrings
>   , testSelectInt64
>   , testSelectNoRows
>   , testUnion
>   , testSelectManyRows
>   , testBindString
>   , testBindDouble
>   , testBindDate
>   , testCreateDual
>   ]


> ignoreError action = catchPG action (const (return ()))

> printIgnoreError action = catchPG action (putStrLn . show)

> printPropagateError action = catchPG action 
>     (\e -> putStrLn (show e) >> rethrowPG e)

> ddlExec db stmt = nqExec db stmt >> return ()

> testOpen connstr = openDb connstr >>= closeDb

> createFixture db = do
>   -- ignoreError $ ddlExec db "drop table tdual"
>   -- ignoreError $ ddlExec db "drop table t_natural"
>   -- ignoreError $ ddlExec db "drop table t_blob"
>   printPropagateError $
>       ddlExec db "create temp table tdual (dummy text primary key)"
>   printPropagateError $
>       ddlExec db "insert into tdual (dummy) values ('X')"
>   printPropagateError $ ddlExec db 
>		    "create temp table t_natural (n integer primary key)"
>   mapM_ (insertNatural db) [1,2,64,65534,65535,65536]
>   --mapM_ (insertNatural db) [1..65536]
>   printPropagateError $
>       ddlExec db "create temp table t_blob (b bytea)"
>   printPropagateError $
>       ddlExec db "insert into t_blob values ('blobtest')"

> insertNatural db n = do
>   ddlExec db $ "insert into t_natural values (" ++ (show n) ++ ")"



> destroyFixture db = return () -- our tables are temporary

> testCreateDual db = do
>   ignoreError ( do
>       ddlExec db "create temp table tdual (dummy integer primary key)"
>       assertFailure "PGException not thrown when table already exists"
>     )
>   printPropagateError $ ddlExec db "insert into tdual values (1)"


> testSelectStrings db = do
>   sn <- printPropagateError $
>     stmtPrepare db "" "select text(n) from t_natural where n < 3 order by n;" []
>   (stmt,ntuples) <- stmtExec0 db sn
>   assertEqual "testSelectInts: ntuples" 2 ntuples
>   --fmt0 <- fPQfformat stmt 0
>   --ct0  <- fPQftype stmt 0
>   --putStrLn $ "\nt_natural: format " ++ (show fmt0) ++ ", type (oid) " ++ (show ct0)
>   n <- colValString stmt 1 1
>   assertEqual "testSelectInts: 1" 1 (read n)
>   n <- colValString stmt 2 1
>   assertEqual "testSelectInts: 2" 2 (read n)
>   stmtFinalise stmt

> testSelectInts db = do
>   sn <- printPropagateError $
>     stmtPrepare db "" "select n from t_natural where n < 65536 order by n;" []
>   (rs,ntuples) <- stmtExec0 db sn
>   --assertEqual "testSelectInts: ntuples" 65535 ntuples
>   assertEqual "testSelectInts: ntuples" 5 ntuples
>   --fmt0 <- fPQfformat stmt 0
>   --ct0  <- fPQftype stmt 0
>   --putStrLn $ "\nt_natural: format " ++ (show fmt0) ++ ", type (oid) " ++ (show ct0)
>   n <- colValInt rs 1 1
>   assertEqual "testSelectInts: 1" 1 n
>   n <- colValInt rs 2 1
>   assertEqual "testSelectInts: 2" 2 n
>   n <- colValInt rs 3 1
>   assertEqual "testSelectInts: 64" 64 n
>   n <- colValInt rs 4 1
>   assertEqual "testSelectInts: 65534" 65534 n
>   stmtFinalise rs

> testSelectInt64 db = do
>   sn <- printPropagateError $
>     stmtPrepare db "" "select 20041225235959" []
>     --stmtPrepare db "" "select -1 union select 1 union select 2*1000*1000*1000 order by 1"
>   (stmt,ntuples) <- stmtExec0 db sn
>   n <- colValInt64 stmt 1 1
>   assertEqual "testSelectInt64: 20041225235959" 20041225235959 n
>   stmtFinalise stmt


> testSelectDouble db = do
>   sn <- printPropagateError $ stmtPrepare db "" "select 1.2::float8" []
>   (stmt,ntuples) <- stmtExec0 db sn
>   n <- colValDouble stmt 1 1
>   assertEqual "testSelectDouble: 1.2" 1.2 n
>   stmtFinalise stmt

> testSelectDate db = do
>   sn <- printPropagateError $ stmtPrepare db ""
>     ("select timestamp with time zone '2000-01-01'"
>      ++ " union select timestamp with time zone '2001-01-01'"
>      ++ " union select timestamp with time zone '1999-01-01' order by 1"
>     )
>     []
>   let
>     d1 = UTCTime (fromGregorian 1999 1 1) 0
>     d0 = UTCTime (fromGregorian 2000 1 1) 0
>     d2 = UTCTime (fromGregorian 2001 1 1) 0
>     diff1 = realToFrac (diffUTCTime d1 d0)
>     diff2 = realToFrac (diffUTCTime d2 d0)
>   (stmt,ntuples) <- stmtExec0 db sn
>   n <- colValDouble stmt 1 1
>   assertEqual "testSelectDate: 1999" diff1 n
>   d <- colValUTCTime stmt 1 1
>   assertEqual "testSelectDate: 1999" d1 d
>   n <- colValDouble stmt 2 1
>   assertEqual "testSelectDate: 2000" 0.0 n
>   d <- colValUTCTime stmt 2 1
>   assertEqual "testSelectDate: 2000" d0 d
>   n <- colValDouble stmt 3 1
>   assertEqual "testSelectDate: 2001" diff2 n
>   d <- colValUTCTime stmt 3 1
>   assertEqual "testSelectDate: 2001" d2 d
>   stmtFinalise stmt

> mkUTCTime :: Integral a => a -> a -> a -> a -> a -> a -> UTCTime
> mkUTCTime year month day hour minute second =
>   localTimeToUTC (hoursToTimeZone 0)
>     (LocalTime
>       (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day))
>       (TimeOfDay (fromIntegral hour) (fromIntegral minute) (fromIntegral second)))


Here we test some funny date boundary cases in Postgres.
There's some funnyness around 1916-10-01 02:25:20 when we use time zones.

testSelectDate2: 1, -2627156078
testSelectDate2: 2, -2627156079
testSelectDate2: 3, -2627156080 (without time zone)
testSelectDate2: 4, -2627158159 (with time zone)
diff 3-4: 2080 seconds = 00:34:40

> testSelectDate2 db = do
>   let
>     sqltext =  "select timestamp without time zone '1916-10-01 02:25:22', 10"
>      ++ " union select timestamp without time zone '1916-10-01 02:25:21', 20"
>      ++ " union select timestamp without time zone '1916-10-01 02:25:20', 30"
>      ++ " union select timestamp with time zone '1916-10-01 02:25:20' /* this one fails */, 40"
>      ++ " order by 2"
>   sn <- printPropagateError $ stmtPrepare db "" sqltext []
>   let
>     ds = (
>            mkUTCTime 1916 10  1  2 25 22:
>            mkUTCTime 1916 10  1  2 25 21:
>            mkUTCTime 1916 10  1  2 25 20:
>            mkUTCTime 1916 10  1  2 25 20:
>          [] )
>   (stmt,ntuples) <- stmtExec0 db sn
>   let
>     loop n =
>       if n <= ntuples
>         then do
>         x <- colValDouble stmt n 1
>         d <- colValUTCTime stmt n 1
>         assertEqual ("testSelectDate2: " ++ show n ++ "\n" ++ sqltext) (ds!!(n-1)) d
>         loop (n+1)
>         else return ()
>   loop 1
>   stmtFinalise stmt


Postgres only allows specification of -ve years by use of the AD/BC suffix.
Sadly, this differs from the astronomical year number like so:
astro: ...3,2,1,0,-1,-2,-3,...
ad/bc: ...3AD,2AD,1AD,-1BC,-2BC,-3BC,...

i.e. 0 astro = -1BC, -1 astro = -2BC, etc.

ISO8601 uses astronomical years, so we ought to be able to write
-1000-12-25 (instead of 1001-01-01 BC), but Postgres won't parse this.

> testSelectDate3 db = do
>   let
>     sqltext =  "select timestamp without time zone '1900-01-01', 10"
>      ++ " union select timestamp without time zone '1000-01-01', 20"
>      ++ " union select timestamp without time zone '0001-01-01', 30"
>      ++ " union select timestamp without time zone '1001-01-01 BC', 40"
>      ++ " order by 2"
>   sn <- printPropagateError $ stmtPrepare db "" sqltext []
>   let
>     ds = (
>            mkUTCTime 1900 1  1  0 0 0:
>            mkUTCTime 1000 1  1  0 0 0 :
>            mkUTCTime 0001 1  1  0 0 0 :
>            mkUTCTime (-1000) 1  1  0 0 0:
>          [] )
>   (stmt,ntuples) <- stmtExec0 db sn
>   let
>     loop n =
>       if n <= ntuples
>         then do
>         x <- colValDouble stmt n 1
>         d <- colValUTCTime stmt n 1
>         assertEqual ("testSelectDate3: " ++ show n ++ "\n" ++ sqltext) (ds!!(n-1)) d
>         loop (n+1)
>         else return ()
>   loop 1
>   stmtFinalise stmt


If we don't specify the type, Postgres gives the number the NUMERIC type.
I don't yet know what the internal binary rep is, nor how to
convert it to an appropriate Haskell type.

> testSelectNumeric db = do
>   sn <- printPropagateError $ stmtPrepare db "" "select 1.2" []
>   (rs,ntuples) <- stmtExec0 db sn
>   fmt0 <- fPQfformat rs 0
>   ct0  <- fPQftype rs 0
>   putStrLn $ "\ntestSelectNumeric: format " ++ (show fmt0) ++ ", type (oid) " ++ (show ct0)
>   --n <- colValDouble rs 1 1
>   --assertEqual "testSelectNumeric: 1.2" 1.2 n
>   stmtFinalise rs



> testUnion db = do
>   (stmt,ntuples) <- printPropagateError $
>     stmtExecImm db "select 'h1' from tdual union select 'h2' from tdual union select 'h3' from tdual" []
>   assertEqual "testUnion: ntuples" 3 ntuples
>   s <- colValString stmt 1 1
>   assertEqual "testUnion: h1" "h1" s
>   s <- colValString stmt 2 1
>   assertEqual "testUnion: h2" "h2" s
>   s <- colValString stmt 3 1
>   assertEqual "testUnion: h3" "h3" s
>   stmtFinalise stmt


> testSelectNoRows db = do
>   (stmt,ntuples) <- printPropagateError $
>     stmtExecImm db "select 'h1' from tdual where dummy = '2'" []
>   stmtFinalise stmt
>   assertEqual "testSelectNoRows: done" 0 ntuples


> manyRows :: String
> manyRows = 
>   "select 1 from"
>   ++ "  ( select 1 from tdual union select 0 from tdual) as t1"
>   ++ ", ( select 2 from tdual union select 0 from tdual) as t2"
>   ++ ", ( select 3 from tdual union select 0 from tdual) as t3"
>   ++ ", ( select 4 from tdual union select 0 from tdual) as t4"
>   ++ ", ( select 5 from tdual union select 0 from tdual) as t5"
>   ++ ", ( select 6 from tdual union select 0 from tdual) as t6"
>   ++ ", ( select 7 from tdual union select 0 from tdual) as t7"
>   ++ ", ( select 8 from tdual union select 0 from tdual) as t8"
>   ++ ", ( select 9 from tdual union select 0 from tdual) as t9"
>   ++ ", ( select 10 from tdual union select 0 from tdual) as t10"


> cursor'name = "takusenp"

> countRows :: DBHandle -> String -> Int -> IO Int
> countRows db sn n = do
>   (stmt,ntuples) <- printPropagateError $ stmtExec0 db sn
>   stmtFinalise stmt
>   if ntuples == 0
>     then return n
>     else countRows db sn (n+ntuples)

> testSelectManyRows db = do
>   let prefetch = 200
>   _ <- printPropagateError $ nqExec db "Begin work"
>   let q = "DECLARE " ++ cursor'name ++ " NO SCROLL CURSOR FOR " ++
>		  manyRows
>   _ <- printPropagateError $ nqExec db q
>   let f = "FETCH FORWARD " ++ (show prefetch) ++ " FROM " ++ cursor'name
>   sn <- printPropagateError $ stmtPrepare db "" f []
>   n <- countRows db sn 0
>   _ <- printPropagateError $ nqExec db $ "CLOSE " ++ cursor'name
>   _ <- printPropagateError $ nqExec db $ "commit work"
>   assertEqual "testSelectManyRows: done" 1024 n


> testBindString db = do
>   (rs, ntuples) <- printPropagateError $
>     prepare'n'exec db "" (substituteBindPlaceHolders "select ? from tdual") [newBindVal "h1"]
>   n <- colValString rs 1 1
>   assertEqual "testBindString: h1" "h1" n
>   stmtFinalise rs


> testBindDouble db = do
>   let
>     v1 :: Double; v1 = 2.3
>     v2 :: Int; v2 = 2001
>     v3 :: Int; v3 = 2001
>     bindvals = [newBindVal v1, newBindVal v2, newBindVal v3]
>   (rs, ntuples) <- printPropagateError $
>     prepare'n'exec db "" (substituteBindPlaceHolders "select ? from tdual where ? = ?") bindvals
>   n <- colValDouble rs 1 1
>   assertEqual "testBindDouble: 2.3" 2.3 n
>   stmtFinalise rs

> testBindDate db = do
>   let
>     v1 :: UTCTime; v1 = UTCTime (fromGregorian 2000 1 1) 0
>     v2 :: UTCTime; v2 = UTCTime (fromGregorian 1980 2 29) 0
>     bindvals = [newBindVal v1, newBindVal v2]
>   (rs, ntuples) <- printPropagateError $
>     prepare'n'exec db "" (substituteBindPlaceHolders "select ?, ?") bindvals
>   n <- colValUTCTime rs 1 1
>   assertEqual "testBindDate: 1 " v1 n
>   n <- colValUTCTime rs 1 2
>   assertEqual "testBindDate: 2 " v2 n
>   stmtFinalise rs
