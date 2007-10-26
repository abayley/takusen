
|
Module      :  Database.ODBC.Test.OdbcFunctions
Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable



> module Database.ODBC.Test.OdbcFunctions where

> import Database.ODBC.OdbcFunctions
> import Control.Exception (finally)
> import Data.Char
> import Data.List
> import Data.Time
> import Data.Word (Word8)
> import Database.Util
> import Test.MiniUnit
> import Foreign.ForeignPtr (withForeignPtr)
> import Foreign.Ptr (castPtr)
> import Foreign.Storable (peek)
> import Foreign.Marshal.Array (peekArray0, peekArray)
> import Numeric (showHex)


> ignoreError action =
>   catchOdbc action (\e -> return undefined)

> printIgnoreError action = catchOdbc action 
>     (\e -> do
>       putStrLn (show e)
>       return undefined
>     )

> printPropagateError action = catchOdbc action 
>     (\e -> do
>       putStrLn (show e)
>       throwOdbc e
>       return undefined
>     )

> testCreateEnv = do
>   env <- allocEnv
>   freeEnv env

> testCreateConn = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   freeConn conn
>   freeEnv env

> testConnect connstr = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   connstr <- connect conn connstr
>   disconnect conn
>   freeConn conn
>   freeEnv env

> createConn connstr = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   connstr <- connect conn connstr
>   return (env, conn)

> closeConn (env, conn) = do
>   disconnect conn
>   freeConn conn
>   freeEnv env


> testCreateStmt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 'x'"
>   executeStmt stmt
>   freeStmt stmt

> createDual conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "create table tdual (dummy varchar(1) primary key)"
>   executeStmt stmt
>   prepareStmt stmt "insert into tdual values ('X')"
>   executeStmt stmt
>   freeStmt stmt

> dropDual conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "drop table tdual"
>   executeStmt stmt
>   freeStmt stmt


> testFetchString conn = do
>   stmt <- allocStmt conn
>   let string1 = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   let string2 = "xyz" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   prepareStmt stmt ("select '" ++ string1
>     ++ "' union select '" ++ string2 ++ "' order by 1")
>   executeStmt stmt
>   more <- fetch stmt
>   s <- getDataUTF8String stmt 1
>   assertEqual "testFetchString" (Just string1) s
>   more <- fetch stmt
>   s <- getDataUTF8String stmt 1
>   assertEqual "testFetchString" (Just string2) s
>   more <- fetch stmt
>   assertBool "testFetchString: EOD" (not more)
>   freeStmt stmt

> testFetchStringWithBuffer conn = do
>   stmt <- allocStmt conn
>   let string1 = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   let string2 = "xyz" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   prepareStmt stmt ("select '" ++ string1
>     ++ "' from tdual union select '" ++ string2 ++ "' from tdual order by 1")
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testFetchStringWithBuffer" (Just string1) s
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testFetchStringWithBuffer" (Just string2) s
>   more <- fetch stmt
>   assertBool "testFetchStringWithBuffer: EOD" (not more)
>   freeStmt stmt

> testFetchInt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 101 from tdual"
>   executeStmt stmt
>   more <- fetch stmt
>   s <- getData stmt 1
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchIntWithBuffer conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 101 from tdual"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Int))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchDouble conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 123.456789 from tdual"
>   executeStmt stmt
>   --buffer <- bindColBuffer stmt 1 sqlDTypeDouble 16
>   more <- fetch stmt
>   --s <- getDoubleFromBuffer buffer
>   s <- getData stmt 1
>   let expect :: Double; expect = 123.456789
>   assertEqual "testFetchDouble" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchDouble: EOD" (not more)
>   freeStmt stmt

> testFetchDatetime conn = do
>   stmt <- allocStmt conn
>   flip finally (freeStmt stmt) ( do
>     -- There is no common SQL standard for datetime literal text.
>     -- Well, there is (timestamp), but MS SQL Server doesn't support it. Pah.
>     prepareStmt stmt "select cast ('1916-10-01 02:25:21' as timestamp) from tdual"
>     --prepareStmt stmt "select cast ('1916-10-01 02:25:21' as datetime) from tdual"
>     executeStmt stmt
>     let expect :: UTCTime; expect = mkUTCTime 1916 10  1  2 25 21
>     buffer <- bindColBuffer stmt 1 0 (Just expect)
>     more <- fetch stmt
>     t <- getUtcTimeFromBuffer buffer
>     --t <- getDataUtcTime stmt 1
>     assertEqual "testFetchDatetime" (Just expect) t
>     more <- fetch stmt
>     assertBool "testFetchDatetime: EOD" (not more)
>     )


> testBindInt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   bindParamBuffer stmt 1 (Just (101::Int))
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 0 (Just (0::Int))
>   more <- fetch stmt
>   s <- getFromBuffer buffer
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testBindString conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   bindParamBuffer stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 1000 (Just expect)
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testBindString (PostgreSQL fails this one)" (Just expect) s
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   freeStmt stmt

> printBufferContents buffer = do
>   withForeignPtr (bindBufPtr buffer) $ \bptr -> do
>   withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
>   sz <- peek szptr
>   putStrLn ("printBufferContents: sz = " ++ show sz)
>   l <- peekArray (fromIntegral sz) (castPtr bptr)
>   let toHex :: Word8 -> String; toHex i = showHex i ""
>   let h :: [String]; h = map toHex l
>   putStrLn (concat (intersperse " " h))

> testUTF8 conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "drop table t_utf8"
>   ignoreError (executeStmt stmt)
>   prepareStmt stmt "create table t_utf8(s text)"
>   executeStmt stmt
>   let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   prepareStmt stmt ("insert into t_utf8 values ( '" ++ expect ++ "' )")
>   executeStmt stmt
>   prepareStmt stmt "select s from t_utf8"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just expect)
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testUTF8" (Just expect) s
>   freeStmt stmt

> testBindUTCTime conn = do
>   stmt <- allocStmt conn
>   flip finally (freeStmt stmt) ( do
>     prepareStmt stmt "select ? from tdual"
>     let expect :: UTCTime; expect = mkUTCTime 1916 10  1  2 25 21
>     bindbuf <- bindParamBuffer stmt 1 (Just expect)
>     executeStmt stmt
>     buffer <- bindColBuffer stmt 1 undefined (Just expect)
>     more <- fetch stmt
>     t <- getUtcTimeFromBuffer buffer
>     assertEqual "testBindUTCTime" (Just expect) t
>     more <- fetch stmt
>     assertBool "testBindUTCTime: EOD" (not more)
>     )


> testBindUTCTimeBoundary conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ?, ? from tdual"
>   -- 1753 seems to be about the earliest year MS SQL Server supports.
>   let expect1 :: UTCTime; expect1 = mkUTCTime 1753 1 1 0 0 0
>   let input1 = expect1
>   let expect2 :: UTCTime; expect2 = mkUTCTime 9999 10  1  2 25 21
>   let input2 = expect2
>   bindParamBuffer stmt 1 (Just input1)
>   bindParamBuffer stmt 2 (Just input2)
>   executeStmt stmt
>   buffer1 <- bindColBuffer stmt 1 32 (Just expect1)
>   buffer2 <- bindColBuffer stmt 2 32 (Just expect1)
>   more <- fetch stmt
>   t1 <- getUtcTimeFromBuffer buffer1
>   t2 <- getUtcTimeFromBuffer buffer2
>   assertEqual "testBindUTCTimeBoundary1" (Just expect1) t1
>   assertEqual "testBindUTCTimeBoundary2" (Just expect2) t2
>   more <- fetch stmt
>   assertBool "testBindUTCTimeBoundary: EOD" (not more)
>   freeStmt stmt

> testRebind conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   --
>   let expect = "abc"
>   bindParamBuffer stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testRebind1" (Just expect) s
>   --
>   closeCursor stmt
>   --
>   let expect = "xyz"
>   bindParamBuffer stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testRebind2" (Just expect) s
>   --
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   freeStmt stmt


> testIsolationLevel conn = do
>   -- For PostgreSQL this fails with:
>   -- 206 HY009 Illegal parameter value for SQL_TXN_ISOLATION
>   --setTxnIsolation conn sqlTxnReadUncommitted
>   setTxnIsolation conn sqlTxnReadCommitted  -- This is OK
>   -- For PostgreSQL this fails with:
>   -- 206 HY009 Illegal parameter value for SQL_TXN_ISOLATION
>   --setTxnIsolation conn sqlTxnRepeatableRead
>   setTxnIsolation conn sqlTxnSerializable  -- This is OK


> testlist =
>   testCreateStmt :
>   testIsolationLevel :
>   testFetchString :
>   testFetchStringWithBuffer :
>   testFetchInt :
>   testFetchIntWithBuffer :
>   testFetchDouble :
>   testFetchDatetime :
>   testBindInt :
>   testBindString :
>   testBindUTCTime :
>   testBindUTCTimeBoundary :
>   testUTF8 :
>   testRebind :
>   []

> mkTestlist conn testlist = map (\testcase -> printIgnoreError (testcase conn)) testlist

> parseArgs :: [String] -> IO String
> parseArgs args = do
>    let (dsn:_) = args
>    return dsn

> runTest :: [String] -> IO ()
> runTest as = do
>   connstr <- parseArgs as
>   printPropagateError testCreateEnv
>   printPropagateError testCreateConn
>   printPropagateError (testConnect connstr)
>   (env, conn) <- printPropagateError (createConn connstr)
>   ignoreError (dropDual conn)
>   printPropagateError (createDual conn)
>   counts <- runTestTT "ODBC low-level tests" (mkTestlist conn testlist)
>   printPropagateError (dropDual conn)
>   closeConn (env, conn)
>   return ()
