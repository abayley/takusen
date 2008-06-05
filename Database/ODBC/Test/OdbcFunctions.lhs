
|
Module      :  Database.ODBC.Test.OdbcFunctions
Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable


-- FIXME  add tests for moreResults

> module Database.ODBC.Test.OdbcFunctions where

> import Database.ODBC.OdbcFunctions
> import Control.Exception (finally)
> import Control.Monad (liftM, when)
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
>   dbms <- getInfoDbmsName conn
>   return (env, conn { connDbms = map toLower dbms } )

> closeConn (env, conn) = do
>   disconnect conn
>   freeConn conn
>   freeEnv env


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


> testCreateStmt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 'x' from tdual"
>   executeStmt stmt
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


Uses getData, rather than a buffer.

> testFetchString conn = do
>   stmt <- allocStmt conn
>   let string1 = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   let string2 = "xyz" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   prepareStmt stmt ("select '" ++ string1
>     ++ "' from tdual union select '" ++ string2 ++ "' from tdual order by 1")
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
>   --s <- getFromBuffer buffer
>   s <- getData stmt 1
>   let expect :: Double; expect = 123.456789
>   assertEqual "testFetchDouble" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchDouble: EOD" (not more)
>   freeStmt stmt

> testFetchDatetime conn = do
>   stmt <- allocStmt conn
>   dbmsname <- liftM (map toLower) (getInfoDbmsName conn)
>   flip finally (freeStmt stmt) ( do
>     -- There is no common SQL standard for datetime literal text.
>     -- Well, there is (timestamp), but MS SQL Server doesn't support it. Pah.
>     -- And Oracle seems to also require a sane NLS setting e.g.
>     --   alter session set NLS_TIMESTAMP_FORMAT = 'yyyy-mm-dd hh24:mi:ss'
>     when (dbmsname == "oracle") ( do
>         prepareStmt stmt "alter session set NLS_TIMESTAMP_FORMAT = 'yyyy-mm-dd hh24:mi:ss'"
>         executeStmt stmt
>       )
>     --
>     -- PostgreSQL doesn't appear to support the {fn ...} escape sequences;
>     -- the text seems to be passed through unchanged (according to SQLNativeSql, anyway).
>     -- THe MS SQL Server ODBC driver's SQLNativeSql also returns the text unchanged,
>     -- but it doesn't seem to cause any grief on the server.
>     --getNativeSql conn "select {fn CONVERT('1916-10-01 02:25:21', SQL_TIMESTAMP)}, {fn CONVERT('2005-10-01 00:00:00', SQL_TIMESTAMP)} from tdual" >>= putStrLn
>     case dbmsname of
>       "postgresql" -> 
>         prepareStmt stmt "select cast ('1916-10-01 02:25:21' as timestamp), cast ('2005-10-01 00:00:00' as timestamp) from tdual"
>       otherwise ->
>         prepareStmt stmt "select {fn CONVERT('1916-10-01 02:25:21', SQL_TIMESTAMP)}, {fn CONVERT('2005-10-01 00:00:00', SQL_TIMESTAMP)} from tdual"
>     executeStmt stmt
>     let expect1 = mkUTCTime 1916 10  1  2 25 21
>     let expect2 = mkUTCTime 2005 10  1  0  0  0
>     buffer1 <- bindColBuffer stmt 1 0 (Just expect1)
>     buffer2 <- bindColBuffer stmt 2 0 (Just expect2)
>     more <- fetch stmt
>     t1 <- getFromBuffer buffer1
>     t2 <- getFromBuffer buffer2
>     assertEqual "testFetchDatetime1" (Just expect1) t1
>     assertEqual "testFetchDatetime2" (Just expect2) t2
>     more <- fetch stmt
>     assertBool "testFetchDatetime: EOD" (not more)
>     )


> testBindInt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   bindParamBuffer stmt 1 (Just (101::Int)) 0
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
>   let expect = "abcdefghijklmnopqrstuvwxyz"
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 1000 (Just expect)
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testBindString" (Just expect) s
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   freeStmt stmt

> testBindStringUTF8 conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ? from tdual"
>   let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 1000 (Just expect)
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testBindStringUTF8 (PostgreSQL fails this one)" (Just expect) s
>   more <- fetch stmt
>   assertBool "testBindStringUTF8: EOD" (not more)
>   freeStmt stmt

> testBindUTCTime conn = do
>   stmt <- allocStmt conn
>   flip finally (freeStmt stmt) ( do
>     prepareStmt stmt "select ? from tdual"
>     let expect :: UTCTime; expect = mkUTCTime 1971 10  1  2 25 21
>     bindbuf <- bindParamBuffer stmt 1 (Just expect) 0
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
>   let expect1 = mkUTCTime 1753 1 1 0 0 0
>   let expect2 = mkUTCTime 9999 10  1  2 25 21
>   let input1 = expect1
>   let input2 = expect2
>   inbuf1 <- bindParamBuffer stmt 1 (Just input1) 0
>   inbuf2 <- bindParamBuffer stmt 2 (Just input2) 0
>   executeStmt stmt
>   buffer1 <- bindColBuffer stmt 1 100 (Just expect1)
>   buffer2 <- bindColBuffer stmt 2 100 (Just expect2)
>   more <- fetch stmt
>   t1 <- getFromBuffer buffer1
>   t2 <- getFromBuffer buffer2
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
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testRebind1" (Just expect) s
>   --
>   closeCursor stmt
>   --
>   let expect = "xyz"
>   bindParamBuffer stmt 1 (Just expect) 0
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 100 (Just "")
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testRebind2" (Just expect) s
>   --
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   freeStmt stmt


> testUTF8 conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "drop table t_utf8"
>   ignoreError (executeStmt stmt)
>   prepareStmt stmt "create table t_utf8(s varchar(50))"
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

> testDbmsName conn = do
>   (liftM ("dbms-name: " ++) (getInfoDbmsName conn)) >>= putStrLn
>   (liftM ("dbms-ver: " ++) (getInfoDbmsVer conn)) >>= putStrLn
>   (liftM ("db-name: " ++) (getInfoDatabaseName conn)) >>= putStrLn
>   (liftM ("driver-name: " ++) (getInfoDriverName conn)) >>= putStrLn
>   (liftM ("driver-ver: " ++) (getInfoDriverVer conn)) >>= putStrLn


> printBufferContents buffer = do
>   withForeignPtr (bindBufPtr buffer) $ \bptr -> do
>   withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
>   sz <- peek szptr
>   putStrLn ("printBufferContents: sz = " ++ show sz)
>   l <- peekArray (fromIntegral sz) (castPtr bptr)
>   let
>     toHex :: Word8 -> String;
>     toHex i = (if i < 16 then "0" else "") ++ showHex i ""
>   --let h :: [String]; h = map toHex l
>   putStrLn (concat (intersperse " " (map toHex l)))
>   let toChar :: Word8 -> String; toChar i = if i > 31 && i < 127 then [chr (fromIntegral i)] else " "
>   putStrLn (concat (intersperse "  " (map toChar l)))


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
>   testBindStringUTF8 :
>   testBindUTCTime :
>   testBindUTCTimeBoundary :
>   testUTF8 :
>   testRebind :
>   --testDbmsName :
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
