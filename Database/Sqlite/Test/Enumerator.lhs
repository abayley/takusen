s
|
Module      :  Database.Sqlite.Test.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple test harness. Would like to write proper HUnit or Quickcheck tests in future.
 
Demonstrates possible usage.


> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Sqlite.Test.Enumerator (runTest) where

> import Database.Sqlite.Enumerator
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


> selectS0 :: SessionQuery
> selectS0 = do
>   liftIO $ putStrLn "select no rows"
>   r <- doQuery "select dummy from dual where dummy = 'a' or dummy = '2' " iter []
>   liftIO $ putStrLn (show r)
>   where
>     -- To illustrate that the full signature is not necessary as long
>     -- as some type information (e.g., (c1::String)) is provided --
>     -- or enough context for the compiler to figure that out.
>     --iter :: (Monad m) => String -> IterAct m [String]
>     iter (c1::String) acc = result $ c1:acc

The following test illustrates doing IO in the iteratee itself.
The seed is unit.

> selectS01 :: SessionQuery
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


> selectS1 :: SessionQuery
> selectS1 = do
>   r <- doQuery "select 'hello1' from dual union select 'hello2' from dual union select 'hello3' from dual" iter []
>   liftIO $ putStrLn $ "selectS1: " ++(show r)
>   where
>     -- This is the example of enough context being provided so that
>     -- no signature for the iteratee is necessary.
>     --iter :: (Monad m) => String -> IterAct m [String]
>     iter c1 acc = if c1 == "hello2" then return$ Left (c1:acc)
>                                     else result (c1:acc)


> selectF1I1 :: SessionQuery
> selectF1I1 = do
>   r <- doQuery "select 4841.3403490431, -22340234 from dual union select 33311.32332, 23789234 from dual" iter []
>   liftIO $ putStrLn $ "selectF1I1: " ++ (show r)
>   where
>     iter :: (Monad m) => Double -> Int -> IterAct m [(Double, Int)]
>     iter c1 c2 acc = result $ (c1, c2):acc


> selectS3 :: SessionQuery
> selectS3 = do
>   r <- doQuery "select 'hello1', 'hello2', null from dual" iter []
>   liftIO $ putStrLn $ "selectS3: " ++ (show r)
>   where
>     iter :: (Monad m) => String -> String -> Maybe String
>                          -> IterAct m [(String, String, String)]
>     iter c1 c2 c3 acc = result $ (c1, c2, ifNull c3 "<NULL>"):acc


> selectS2I1 :: SessionQuery
> selectS2I1 = do
>   r <- doQuery "select 'hello1', 'hello2', null from dual" iter []
>   liftIO $ putStrLn $ "selectS2I1: " ++ (show r)
>   where
>     iter :: (Monad m) => String -> String -> Maybe Int
>                          -> IterAct m [(String, String, Int)]
>     iter c1 c2 c3 acc = result $ (c1, c2, ifNull c3 (-(1))):acc



> selectD1 :: SessionQuery
> selectD1 = do
>   r <- doQuery "select 20041225235959 from dual" iter []
>   liftIO $ putStrLn ("[ " ++ (concat (intersperse "\n, " (map showCt r))) ++ " ]")
>   where
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc


> selectD2 :: SessionQuery
> selectD2 = do
>   r <- doQuery qry iter []
>   liftIO $ putStrLn ("[ " ++ (concat (intersperse "\n, " (map showCt r))) ++ " ]")
>   where
>     iter :: (Monad m) => CalendarTime -> IterAct m [CalendarTime]
>     iter c1 acc = result $ c1:acc
>     -- test Oracle date boundary cases
>     qry =       "select  99991231000000 from dual"
>       ++ " union select     10101000000 from dual"
>       ++ " union select    -10101000000 from dual"
>       ++ " union select -47120101000000 from dual"
>       ++ " order by 1 desc"



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
>       ++ " union select null from dual"
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

> createDual :: Session -> IO ()
> createDual sess = do
>   catchDB (
>       runSession (executeDDL "drop table dual") sess
>     ) basicDBExceptionReporter
>   catchDB (
>       runSession (executeDDL "create table dual (dummy varchar(1) primary key)") sess
>     ) basicDBExceptionReporter
>   catchDB (
>       runSession (executeDDL "insert into dual values ('X')") sess
>     ) basicDBExceptionReporter


> createTable :: Session -> IO ()
> createTable sess = catchDB ( do
>     runSession (
>         executeDDL "create table agbtest (id integer, v varchar(10))"
>       ) sess
>   ) basicDBExceptionReporter



> dropTable :: Session -> IO ()
> dropTable sess = catchDB ( do
>     runSession (
>         executeDDL "drop table agbtest"
>       ) sess
>   ) basicDBExceptionReporter



> insertTable :: Int -> String -> SessionQuery
> insertTable n s = do
>   liftIO $ putStrLn ("insert " ++ (show n) ++ " " ++ s)
>   rows <- executeDML $ "insert into agbtest (id, v) values (" ++ (show n) ++ ", '" ++ s ++ "')"
>   return ()

> insertInt :: Int -> SessionQuery
> insertInt n = insertTable n (show n)


> updateTable :: Int -> String -> SessionQuery
> updateTable n s = do
>   rows <- executeDML $ "update agbtest set v = '" ++ s ++ "' where id = " ++ (show n)
>   liftIO $ putStrLn $ "rows updated: " ++ (show rows)

> updateInt :: Int -> Int -> SessionQuery
> updateInt n x = updateTable n (show x)


> executeDDLTest :: Session -> IO ()
> executeDDLTest sess = catchDB ( do
>     putStrLn "\nexecuteDDLTest:"
>     createDual sess
>     dropTable sess
>     createTable sess
>     runSession ( do
>         beginTransaction Serialisable
>         insertInt 1
>         insertInt 2
>         insertInt 3
>         insertInt 2
>         liftIO $ putStrLn "commit"
>         commit
>         beginTransaction Serialisable
>         insertInt 2
>         updateInt 2 22
>         liftIO $ putStrLn "rollback"
>         rollback
>         beginTransaction Serialisable
>         updateInt 2 22
>         liftIO $ putStrLn "commit"
>         commit
>       ) sess
>     dropTable sess
>   ) basicDBExceptionReporter


> -- test polymorphic fetch function
> polymorphicFetchTest :: Session -> IO ()
> polymorphicFetchTest sess = catchDB ( do
>     putStrLn "\npolymorphicFetchTest:"
>     createTable sess
>     runSession ( do
>         beginTransaction Serialisable
>         let
>           l1 :: [Int]
>           l1 = [1, 2, 3, 4]
>           l2 :: [Int]
>           l2 = [5, 6, 7, 8]
>         insertTable 1 (show l1)
>         insertTable 2 (show l2)
>         commit
>         let
>           iter :: (Monad m) => [Int] -> IterAct m [[Int]]
>           iter c1 acc = result $ c1:acc
>         r <- doQuery "select v from agbtest" iter []
>         liftIO $ putStrLn (show r)
>       ) sess
>     dropTable sess
>   ) basicDBExceptionReporter



> allTests :: Session -> IO ()
> allTests sess = do
>     executeDDLTest sess
>     selectTests sess
>     selectNullTest sess
>     selectExhaustCursorTest sess
>     polymorphicFetchTest sess
>     return ()


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
