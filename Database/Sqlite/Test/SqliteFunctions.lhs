> {-# OPTIONS -fglasgow-exts #-}

|
Module      :  Database.Sqlite.Test.SqliteFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 


> module Database.Sqlite.Test.SqliteFunctions (runTest) where


> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Database.Sqlite.SqliteFunctions
> import System.Environment (getArgs)


> nullAction = return ()

> printIgnoreError action =
>   catchSqlite action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )

> printPropagateError action =
>   catchSqlite action 
>     (\e -> do
>       putStrLn (show e)
>       throwSqlite e
>       return undefined
>     )

> ddlExec db stmt = do
>   _ <- stmtExec db stmt
>   return ()

> testOpen dbname = do
>   h <- openDb dbname
>   closeDb h

> testCreateDual db = do
>   printIgnoreError $ ddlExec db "drop table dual"
>   printPropagateError $
>       ddlExec db "create table dual (dummy integer primary key)"
>   printIgnoreError ( do
>       ddlExec db "create table dual (dummy text)"
>       error "SqliteException not thrown when table already exists"
>     )
>   printPropagateError $ ddlExec db "insert into dual values (1)"

> testCreateNatural db = do
>   printIgnoreError $ ddlExec db "drop table natural"
>   printPropagateError $ ddlExec db "create table natural (n integer primary key)"

> insertNatural db n = do
>   _ <- stmtExec db $ "insert into natural values (" ++ (show n) ++ ")"
>   putStrLn $ "insert natural " ++ (show n)

> testInsert db = do
>   mapM_ (insertNatural db) [1..10]


> testSelectInts db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select n from natural where n < 3;"
>   rc <- stmtFetch db stmt
>   n <- colValInt stmt 1
>   rc <- stmtFetch db stmt
>   n <- colValInt stmt 1
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   if rc == sqliteDONE
>     then putStrLn "testSelectInts: ok"
>     else error "Query not finished"


> testSelectInt64 db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 20041225235959"
>     --stmtPrepare db "select -1 union select 1 union select 2*1000*1000*1000 order by 1"
>   rc <- stmtFetch db stmt
>   n <- colValInt64 stmt 1
>   putStrLn $ "testSelectInt64: " ++ (show n)
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   if rc == sqliteDONE
>     then putStrLn "testSelectInt64: ok"
>     else error "Query not finished"


> testSelectDouble db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 1.2"
>   rc <- stmtFetch db stmt
>   n <- colValDouble stmt 1
>   putStrLn $ "testSelectDouble: " ++ (show n)
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   if rc == sqliteDONE
>     then putStrLn "testSelectDouble: ok"
>     else error "Query not finished"



> testUnion db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 'h1' from dual union select 'h2' from dual union select 'h3' from dual"
>   rc <- stmtFetch db stmt
>   s <- colValString stmt 1
>   putStrLn $ "testUnion: " ++ s
>   rc <- stmtFetch db stmt
>   s <- colValString stmt 1
>   putStrLn $ "testUnion: " ++ s
>   rc <- stmtFetch db stmt
>   s <- colValString stmt 1
>   putStrLn $ "testUnion: " ++ s
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   if rc == sqliteDONE
>     then nullAction
>     else error "Query not finished"


> testSelectNoRows db = do
>   stmt <- printPropagateError $
>     stmtPrepare db "select 'h1' from dual where dummy = '2'"
>   rc <- stmtFetch db stmt
>   stmtFinalise db stmt
>   if rc == sqliteDONE
>     then putStrLn "testSelectNoRows: ok"
>     else error "Query not finished"


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


> countRows :: DBHandle -> StmtHandle -> Int -> IO Int
> countRows db stmt n = do
>   rc <- printPropagateError $ stmtFetch db stmt
>   _ <- colValInt stmt 1
>   if rc == sqliteDONE
>     then return n
>     else countRows db stmt (n+1)

> testSelectManyRows db = do
>   stmt <- printPropagateError $ stmtPrepare db manyRows
>   n <- countRows db stmt 0
>   stmtFinalise db stmt
>   putStrLn $ "rowcount " ++ (show n)


> getDbName = do
>   [n] <- getArgs
>   return n


> runTest = do
>   dbname <- getDbName
>   testOpen dbname
>   db <- openDb dbname
>   testCreateDual db
>   testCreateNatural db
>   testInsert db
>   testSelectInts db
>   testSelectDouble db
>   testSelectInt64 db
>   testSelectNoRows db
>   testSelectManyRows db
>   testUnion db
>   closeDb db
