
|
Module      :  Database.Oracle.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple test harness. Would like to write proper HUnit or Quickcheck tests in future.
 
Demonstrates possible usage.

> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Oracle.Test.Enumerator (runTest) where

> import Database.Oracle.Enumerator
> import System.Environment (getArgs)
> import System.Time  -- CalendarTime
> import Data.List (intersperse)


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
-- ** Select tests
--------------------------------------------------------------------

An auxiliary, convenient function, to be used if the body
of the iteratee is pure.


> selectS0 = do
>   r <- doQuery "select dummy from dual where dummy = 'a'" iter []
>   liftIO $ putStrLn (show r)
>   where
>     -- To illustrate that the full signature is not necessary as long
>     -- as some type information (e.g., (c1::String)) is provided --
>     -- or enough context for the compiler to figure that out.
>     --iter :: (Monad m) => String -> IterAct m [String]
>     iter (c1::String) acc = result $ c1:acc

The following test illustrates doing IO in the iteratee itself. The seed
is unit.

> selectS01 = do
>   r <- doQuery "select 'hello1', 'hello2', null from dual" iter ()
>   liftIO $ putStrLn $ "selectS01: " ++ (show r)
>   where
>     -- Here, it's better to avoid the signature and specify
>     -- types of the arguments specifically so we do not have to
>     -- figure out which exactly Monad we're operating in.
>     -- We let the compiler figure out the right monad from
>     -- the context (that is, from the Session)
>     --iter :: String -> IterAct  (ReaderT Query SessionQuery) ()
>     iter (c1::String) () = do
>                   liftIO$ putStrLn $ "In Iteratee: " ++ c1
>                   result ()


> selectS1 = do
>   r <- doQuery "select 'hello1' from dual union select 'hello2' from dual union select 'hello3' from dual" iter []
>   liftIO $ putStrLn $ "selectS1: " ++(show r)
>   where
>     -- This is the example of enough context being provided so that
>     -- no signature for the iteratee is necessary.
>     --iter :: (Monad m) => String -> IterAct m [String]
>     iter c1 acc = if c1 == "hello2" then return$ Left (c1:acc)
>                                     else result (c1:acc)


> selectF1I1 = do
>   r <- doQuery "select 4841.3403490431, -22340234 from dual union select 33311.32332, 23789234 from dual" iter []
>   liftIO $ putStrLn $ "selectF1I1: " ++ (show r)
>   where
>     iter :: (Monad m) => Double -> Int -> IterAct m [(Double, Int)]
>     iter c1 c2 acc = result $ (c1, c2):acc


> selectS3 = do
>   r <- doQuery "select 'hello1', 'hello2', null from dual" iter []
>   liftIO $ putStrLn $ "selectS3: " ++ (show r)
>   where
>     iter :: (Monad m) => String -> String -> Maybe String
>                          -> IterAct m [(String, String, String)]
>     iter c1 c2 c3 acc = result $ (c1, c2, ifNull c3 "<NULL>"):acc


> selectS2I1 = do
>   r <- doQuery "select 'hello1', 'hello2', null from dual" iter []
>   liftIO $ putStrLn (show r)
>   where
>     iter :: (Monad m) => String -> String -> Maybe Int
>                          -> IterAct m [(String, String, Int)]
>     iter c1 c2 c3 acc = result $ (c1, c2, ifNull c3 (-(1))):acc



> selectD2 = do
>   r <- doQuery qry iter []
>   liftIO $ putStrLn ("[ " ++ (concat (intersperse "\n, " (map showCt r))) ++ " ]")
>   where
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc
>     -- test Oracle date boundary cases
>     qry =       "select to_date('+9999-12-31', 'syyyy-mm-dd') from dual"
>       ++ " union select to_date('+0001-01-01', 'syyyy-mm-dd') from dual"
>       ++ " union select to_date('-0001-01-01', 'syyyy-mm-dd') from dual"
>       ++ " union select to_date('-4712-01-01', 'syyyy-mm-dd') from dual"
>       -- Can't have year 0 - Oracle barfs
>       -- ++ " union select to_date('-0000-01-01', 'syyyy-mm-dd') from dual"
>       -- ++ " union select to_date('+0000-01-01', 'syyyy-mm-dd') from dual"
>       ++ " order by 1 desc"


> selectD1 :: SessionQuery
> selectD1 = do
>   r <- doQuery "select sysdate from dual" iter []
>   liftIO $ putStrLn ("[ " ++ (concat (intersperse "\n, " (map showCt r))) ++ " ]")
>   where
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc



> selectCursor :: SessionQuery
> selectCursor = do
>   let
>     query = "select 1 from dual union select 2 from dual union select 3 from dual"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter i acc = result $ i:acc
>   withCursorBracket query iter [] $ \c -> do
>     liftIO $ putStrLn "cursor opened"
>     r <- cursorCurrent c
>     liftIO $ putStrLn (show r)
>     doneBool <- cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor fetch 2"
>     newc <- cursorNext c
>     r <- cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor fetch 3"
>     newc <- cursorNext c
>     r <- cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor fetch 4"
>     newc <- cursorNext c
>     r <- cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     --
>     liftIO $ putStrLn "cursor close"
>     newc <- cursorNext c
>     r <- cursorCurrent newc
>     liftIO $ putStrLn (show r)
>     doneBool <- cursorIsEOF c
>     liftIO $ putStrLn ("Cursor done:" ++ (show doneBool))
>     return ()



> selectExhaustCursor :: SessionQuery
> selectExhaustCursor = do
>   let
>     query = "select 1 from dual union select 2 from dual"
>     -- Again, here we demonstrate the use of a local argument
>     -- type annotation rather than the complete signature.
>     -- Let the compiler figure out the monad and the seed type.
>     --iter :: (Maybe m) => Maybe Int -> IterAct m [Int]
>     iter (i::Maybe Int) acc = result $ (ifNull i (-(1))):acc
>   withCursorBracket query iter [] $ \c -> do
>       _ <- cursorNext c
>       _ <- cursorNext c
>       _ <- cursorNext c
>       _ <- cursorNext c
>       _ <- cursorNext c
>       _ <- cursorNext c
>       _ <- cursorNext c
>       _ <- cursorNext c
>       return ()


> selectNull :: SessionQuery
> selectNull = do
>   r <- doQuery query iter []
>   liftIO $ putStrLn (show r)
>   where
>     query =     "select 1 from dual"
>       ++ " union select 2 from dual"
>       ++ " union select 3 from dual"
>       ++ " union select to_number(null) from dual"
>     iter :: (Monad m) => Int -> IterAct m [Int]
>     iter c1 acc = result $ c1:acc

> selectTests :: Session -> IO ()
> selectTests sess = catchDB ( do
>     putStrLn "\nselectTests:"
>     runSession ( do
>         selectS0
>         selectS01
>         selectS1
>         selectF1I1
>         selectS3
>         selectS2I1
>         selectD1
>         selectD2
>         selectCursor
>       ) sess
>   ) basicDBExceptionReporter


> selectNullTest :: Session -> IO ()
> selectNullTest sess = catchDB ( do
>     putStrLn "\nselectNullTest:"
>     runSession selectNull sess
>   ) basicDBExceptionReporter


> selectExhaustCursorTest :: Session -> IO ()
> selectExhaustCursorTest sess = catchDB ( do
>     putStrLn "\nselectExhaustCursorTest:"
>     runSession selectExhaustCursor sess
>   ) basicDBExceptionReporter


--------------------------------------------------------------------
-- ** Insert, update tests
--------------------------------------------------------------------

955 - object exists with same name

> createTable :: Session -> IO ()
> createTable sess = ignoreDBError 955 $
>     runSession (
>         executeDDL "create table agbtest (id number, v varchar(10))"
>       ) sess


942 - object does not exist

> dropTable :: Session -> IO ()
> dropTable sess = ignoreDBError 942 $
>     runSession (
>         executeDDL "drop table agbtest"
>       ) sess



> insertTable :: Int -> SessionQuery
> insertTable n = do
>   liftIO $ putStrLn ("insert " ++ (show n))
>   rows <- executeDML $ "insert into agbtest (id, v) values (" ++ (show n) ++ ", '" ++ (show n) ++ "')"
>   return ()

> updateTable :: Int -> Int -> SessionQuery
> updateTable x y = do
>   rows <- executeDML $ "update agbtest set v = '" ++ (show y) ++ "' where id = " ++ (show x)
>   liftIO $ putStrLn $ "rows updated: " ++ (show rows)




> executeDDLTest :: Session -> IO ()
> executeDDLTest sess = catchDB ( do
>     putStrLn "\nexecuteDDLTest:"
>     dropTable sess
>     createTable sess
>     runSession ( do
>         insertTable 1
>         insertTable 2
>         insertTable 3
>         insertTable 2
>         liftIO $ putStrLn "commit"
>         commit
>         insertTable 2
>         updateTable 2 22
>         liftIO $ putStrLn "rollback"
>         rollback
>         updateTable 2 22
>         liftIO $ putStrLn "commit"
>         commit
>       ) sess
>     dropTable sess
>   ) basicDBExceptionReporter


> allTests :: Session -> IO ()
> allTests sess = do
>     executeDDLTest sess
>     selectTests sess
>     selectNullTest sess
>     selectExhaustCursorTest sess


> argLogon :: IO Session
> argLogon = do
>   [ user, pswd, dbname ] <- getArgs
>   connect user pswd dbname


> runTest :: IO ()
> runTest = catchDB ( do
>     sess <- argLogon
>     allTests sess
>     disconnect sess
>   ) basicDBExceptionReporter
