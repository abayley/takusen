{-|
Module      :  Database.Test.SimpleEnumeratorTest
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainers :  oleg@pobox.com, alistair@abayley.org
Stability   :  unstable
Portability :  non-portable

Simple test harness. Would like to write proper HUnit or Quickcheck tests in future.

Demonstrates possible usage.
-}


> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Test.SimpleEnumeratorTest where

> import qualified Database.Enumerator as DB
> import qualified Database.Oracle.Enumerator as DB
> import Database.Oracle.Enumerator (liftIO, throwIO)
> import System.Environment (getArgs)
> import System.Time  -- CalendarTime
> import Data.List (intersperse)
> import qualified Database.Oracle.Test.OCIFunctions as TestOci


> zeroPad :: Int -> Int -> String
> zeroPad n i =
>   if i < 0
>   then "-" ++ (zeroPad n (abs i))
>   else (take taken (repeat '0')) ++ (show i)
>   where
>     showi = show i
>     taken = if leni > n then 0 else n - leni
>     leni = length showi

> --instance Show CalendarTime where
> showCt :: CalendarTime -> String
> --showCt ct = show ct
> showCt ct =
>     (zeroPad 4 (ctYear ct))
>     ++ "-" ++ (zeroPad 2 (1 + fromEnum (ctMonth ct)))
>     ++ "-" ++ (zeroPad 2 (ctDay ct))
>     ++ " " ++ (zeroPad 2 (ctHour ct))
>     ++ ":" ++ (zeroPad 2 (ctMin ct))
>     ++ ":" ++ (zeroPad 2 (ctSec ct))


--------------------------------------------------------------------
-- test
--------------------------------------------------------------------



> selectS0 = do
>   r <- DB.doQuery "select dummy from dual where dummy = 'a'" iter []
>   liftIO $ putStrLn (show r)
>   where
>     iter :: String -> DB.IterAct [String]
>     iter c1 acc = Right $ c1:acc


> selectS1 = do
>   r <- DB.doQuery "select 'hello1' from dual union select 'hello2' from dual union select 'hello3' from dual" iter []
>   liftIO $ putStrLn (show r)
>   where
>     iter :: String -> DB.IterAct [String]
>     iter c1 acc = if c1 == "hello2" then Left (c1:acc) else Right (c1:acc)


> selectF1I1 = do
>   r <- DB.doQuery "select 4841.3403490431, -22340234 from dual union select 33311.32332, 23789234 from dual" iter []
>   liftIO $ putStrLn (show r)
>   where
>     iter :: Double -> Int -> DB.IterAct [(Double, Int)]
>     iter c1 c2 acc = Right $ (c1, c2):acc


> selectS3 = do
>   r <- DB.doQuery "select 'hello1', 'hello2', null from dual" iter []
>   liftIO $ putStrLn (show r)
>   where
>     iter :: String -> String -> Maybe String -> DB.IterAct [(String, String, String)]
>     iter c1 c2 c3 acc = Right $ (c1, c2, DB.ifNull c3 "<NULL>"):acc


> selectS2I1 = do
>   r <- DB.doQuery "select 'hello1', 'hello2', null from dual" iter []
>   liftIO $ putStrLn (show r)
>   where
>     iter :: String -> String -> Maybe Int -> DB.IterAct [(String, String, Int)]
>     iter c1 c2 c3 acc = Right $ (c1, c2, DB.ifNull c3 (-(1))):acc



> selectD2 = do
>   r <- DB.doQuery qry iter []
>   liftIO $ putStrLn ("[ " ++ (concat (intersperse "\n, " (map showCt r))) ++ " ]")
>   where
>     iter :: CalendarTime -> DB.IterAct [CalendarTime]
>     iter c1 acc = Right $ c1:acc
>     qry =       "select to_date('+9999-12-31', 'syyyy-mm-dd') from dual"
>       ++ " union select to_date('+0001-01-01', 'syyyy-mm-dd') from dual"
>       ++ " union select to_date('-0001-01-01', 'syyyy-mm-dd') from dual"
>       ++ " union select to_date('-4712-01-01', 'syyyy-mm-dd') from dual"
>       -- Can't have year 0 - Oracle barfs
>       -- ++ " union select to_date('-0000-01-01', 'syyyy-mm-dd') from dual"
>       -- ++ " union select to_date('+0000-01-01', 'syyyy-mm-dd') from dual"
>       ++ " order by 1 desc"


> selectD1 :: DB.SessionQuery
> selectD1 = do
>   r <- DB.doQuery "select sysdate from dual" iter []
>   liftIO $ putStrLn ("[ " ++ (concat (intersperse "\n, " (map showCt r))) ++ " ]")
>   where
>     iter :: CalendarTime -> DB.IterAct [CalendarTime]
>     iter c1 acc = Right $ c1:acc



> selectCursor :: DB.SessionQuery
> selectCursor = do
>   let
>     query = "select 1 from dual union select 2 from dual union select 3 from dual"
>     iter :: Int -> DB.IterAct [Int]
>     iter i acc = Right $ i:acc
>   c <- DB.openCursor query iter []
>   DB.withCursorBracket c $ do
>     liftIO $ putStrLn "cursor opened"
>     r <- DB.cursorCurrent c
>     liftIO $ putStrLn (show r)
>     doneBool <- DB.cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor fetch 2"
>     newc <- DB.cursorNext c
>     r <- DB.cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- DB.cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor fetch 3"
>     newc <- DB.cursorNext c
>     r <- DB.cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- DB.cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor fetch 4"
>     newc <- DB.cursorNext c
>     r <- DB.cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- DB.cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor close"
>     newc <- DB.cursorNext c
>     r <- DB.cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- DB.cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     return ()



> selectExhaustCursor :: DB.SessionQuery
> selectExhaustCursor = do
>   let
>     query = "select 1 from dual union select 2 from dual"
>     iter :: Maybe Int -> DB.IterAct [Int]
>     iter i acc = Right $ (DB.ifNull i (-(1))):acc
>   c <- DB.openCursor query iter []
>   DB.withCursorBracket c $ do
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       _ <- DB.cursorNext c
>       return ()


> selectNull :: DB.SessionQuery
> selectNull = do
>   r <- DB.doQuery query iter []
>   liftIO $ putStrLn (show r)
>   where
>     query =     "select 1 from dual"
>       ++ " union select 2 from dual"
>       ++ " union select 3 from dual"
>       ++ " union select to_number(null) from dual"
>     iter :: Int -> DB.IterAct [Int]
>     iter c1 acc = Right $ c1:acc


955 - object exists with same name

> createTable :: DB.Session -> IO ()
> createTable sess = DB.ignoreDBError 955 $
>     DB.runSession (
>         DB.executeDDL "create table agbtest (id number, v varchar(10))"
>       ) sess


942 - object does not exist

> dropTable :: DB.Session -> IO ()
> dropTable sess = DB.ignoreDBError 942 $
>     DB.runSession (
>         DB.executeDDL "drop table agbtest"
>       ) sess



> insertTable :: Int -> DB.SessionQuery
> insertTable n = do
>   liftIO $ putStrLn ("insert " ++ (show n))
>   rows <- DB.executeDML $ "insert into agbtest (id, v) values (" ++ (show n) ++ ", '" ++ (show n) ++ "')"
>   return ()

> updateTable :: Int -> Int -> DB.SessionQuery
> updateTable x y = do
>   rows <- DB.executeDML $ "update agbtest set v = '" ++ (show y) ++ "' where id = " ++ (show x)
>   liftIO $ putStrLn $ "rows updated: " ++ (show rows)



> selectTests :: DB.Session -> IO ()
> selectTests sess = DB.catchDB ( do
>     putStrLn "\nselectTests:"
>     DB.runSession ( do
>         selectS0
>         selectS1
>         selectF1I1
>         selectS3
>         selectS2I1
>         selectD1
>         selectD2
>         selectCursor
>       ) sess
>   ) DB.basicDBExceptionReporter


> selectNullTest :: DB.Session -> IO ()
> selectNullTest sess = DB.catchDB ( do
>     putStrLn "\nselectNullTest:"
>     DB.runSession selectNull sess
>   ) DB.basicDBExceptionReporter


> selectExhaustCursorTest :: DB.Session -> IO ()
> selectExhaustCursorTest sess = DB.catchDB ( do
>     putStrLn "\nselectExhaustCursorTest:"
>     DB.runSession selectExhaustCursor sess
>   ) DB.basicDBExceptionReporter


> executeDDLTest :: DB.Session -> IO ()
> executeDDLTest sess = DB.catchDB ( do
>     putStrLn "\nexecuteDDLTest:"
>     dropTable sess
>     createTable sess
>     DB.runSession ( do
>         insertTable 1
>         insertTable 2
>         insertTable 3
>         insertTable 2
>         liftIO $ putStrLn "commit"
>         DB.commit
>         insertTable 2
>         updateTable 2 22
>         liftIO $ putStrLn "rollback"
>         DB.rollback
>         updateTable 2 22
>         liftIO $ putStrLn "commit"
>         DB.commit
>       ) sess
>     dropTable sess
>   ) DB.basicDBExceptionReporter


> allTests :: DB.Session -> IO ()
> allTests sess = do
>     executeDDLTest sess
>     selectTests sess
>     selectNullTest sess
>     selectExhaustCursorTest sess


> argLogon :: IO DB.Session
> argLogon = do
>   [ user, pswd, dbname ] <- getArgs
>   DB.connect user pswd dbname


> runSimpleTest :: IO ()
> runSimpleTest = DB.catchDB ( do
>     sess <- argLogon
>     allTests sess
>     DB.disconnect sess
>   ) DB.basicDBExceptionReporter
