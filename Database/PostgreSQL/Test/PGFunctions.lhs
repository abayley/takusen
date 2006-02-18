> {-# OPTIONS -fglasgow-exts #-}

|
Module      :  Database.PostgreSQL.Test.PGFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 


> module Database.PostgreSQL.Test.PGFunctions (runTest) where


> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Database.PostgreSQL.PGFunctions
> import System.Environment (getArgs)
> import Test.HUnit




> runTest :: String -> IO ()
> runTest dbname = do
>   testOpen dbname
>   db <- openDb dbname
>   createFixture db
>   runTestTT (testlist db)
>   destroyFixture db
>   closeDb db


> testlist db = TestList $ map (\t -> TestCase (t db))
>   [ testSelectInts
>   -- , testSelectDouble
>   -- , testSelectInt64
>   , testSelectNoRows
>   , testUnion
>   -- , testSelectManyRows
>   -- , testBindString
>   -- , testBindDouble
>   , testCreateDual
>   ]


> ignoreError action =
>   catchPG action (\e -> return undefined)

> printIgnoreError action = catchPG action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )

> printPropagateError action = catchPG action 
>     (\e -> do
>       putStrLn (show e)
>       rethrowPG e
>     )

> ddlExec db stmt = do
>   _ <- nqExec db stmt
>   return ()

> testOpen dbname = do
>   h <- openDb dbname
>   closeDb h

> createFixture db = do
>   -- ignoreError $ ddlExec db "drop table tdual"
>   -- ignoreError $ ddlExec db "drop table t_natural"
>   -- ignoreError $ ddlExec db "drop table t_blob"
>   printPropagateError $
>       ddlExec db "create temp table tdual (dummy text)"
>   printPropagateError $
>       ddlExec db "insert into tdual (dummy) values ('X')"
>   printPropagateError $ ddlExec db 
>		    "create temp table t_natural (n integer primary key)"
>   mapM_ (insertNatural db) [1..10]
>   printPropagateError $
>       ddlExec db "create temp table t_blob (b bytea)"
>   printPropagateError $
>       ddlExec db "insert into t_blob values ('blobtest')"

> insertNatural db n = do
>   ddlExec db $ "insert into t_natural values (" ++ (show n) ++ ")"



> destroyFixture db = return () -- our tables are temporary
> {-
> destroyFixture db = do
>   printPropagateError $ ddlExec db "drop table tdual"
>   printPropagateError $ ddlExec db "drop table t_natural"
>   printPropagateError $ ddlExec db "drop table t_blob"
> -}

> testCreateDual db = do
>   ignoreError ( do
>       ddlExec db "create temp table tdual (dummy integer primary key)"
>       assertFailure "PGException not thrown when table already exists"
>     )
>   printPropagateError $ ddlExec db "insert into tdual values (1)"




> testSelectInts db = do
>   sn <- printPropagateError $
>     stmtPrepare db "" "select n from t_natural where n < 3 order by n;"
>   (stmt,ntuples) <- stmtExec0 db sn
>   assertEqual "testSelectInts: ntuples" 2 ntuples
>   fmt0 <- fPQfformat stmt 0
>   ct0  <- fPQftype stmt 0
>   putStrLn $ "\nt_natural: format " ++ (show fmt0) ++
>	       ", type (oid) " ++ (show ct0)
>   n <- colValString stmt 1 1
>   assertEqual "testSelectInts: 1" 1 (read n)
>   n <- colValString stmt 2 1
>   assertEqual "testSelectInts: 2" 2 (read n)
>   stmtFinalise stmt

> {-
> testSelectInts db = do
>   sn <- printPropagateError $
>     stmtPrepare db "" "select n from t_natural where n < 3 order by n;"
>   (stmt,ntuples) <- stmtExec0 db sn
>   assertEqual "testSelectInts: ntuples" 2 n
>   n <- colValInt stmt 1
>   assertEqual "testSelectInts: 1" 1 n
>   rc <- stmtFetch db stmt
>   n <- colValInt stmt 1
>   assertEqual "testSelectInts: 2" 2 n
>   rc <- stmtFetch db stmt
>   stmtFinalise stmt
>   assertEqual "testSelectInts: done" sqliteDONE rc



> testSelectInt64 db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 20041225235959"
>     --stmtPrepare db "select -1 union select 1 union select 2*1000*1000*1000 order by 1"
>   rc <- stmtFetch db stmt
>   n <- colValInt64 stmt 1
>   assertEqual "testSelectInt64: 20041225235959" 20041225235959 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectInt64: done" sqliteDONE rc


> testSelectDouble db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 1.2"
>   rc <- stmtFetch db stmt
>   n <- colValDouble stmt 1
>   assertEqual "testSelectDouble: 1.2" 1.2 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   assertEqual "testSelectDouble: done" sqliteDONE rc
>   -}



> testUnion db = do
>   (stmt,ntuples) <- printPropagateError $
>     stmtExecImm db "select 'h1' from tdual union select 'h2' from tdual union select 'h3' from tdual"
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
>     stmtExecImm db "select 'h1' from tdual where dummy = '2'"
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
>   let prefetch = 2
>   _ <- printPropagateError $ nqExec db "Begin work"
>   let q = "DECLARE " ++ cursor'name ++ " NO SCROLL CURSOR FOR " ++
>		  manyRows
>   _ <- printPropagateError $ nqExec db q
>   let f = "FETCH FORWARD " ++ (show prefetch) ++ " FROM " ++ cursor'name
>   sn <- printPropagateError $ stmtPrepare db "" f
>   n <- countRows db sn 0
>   _ <- printPropagateError $ nqExec db $ "CLOSE " ++ cursor'name
>   _ <- printPropagateError $ nqExec db $ "commit work"
>   assertEqual "testSelectManyRows: done" 1024 n


> {-
> testBindString db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select ? from tdual"
>   bindString db stmt 1 "h1"
>   rc <- stmtFetch db stmt
>   n <- colValString stmt 1
>   assertEqual "testBindString: h1" "h1" n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt

> testBindDouble db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select ? from tdual where ? = ?"
>   bindDouble db stmt 1 2.3
>   bindInt db stmt 2 2001
>   bindInt64 db stmt 3 2001
>   rc <- stmtFetch db stmt
>   n <- colValDouble stmt 1
>   assertEqual "testBindDouble: 2.3" 2.3 n
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   -}