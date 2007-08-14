
> module Database.ODBC.Test.OdbcFunctions where

> import Database.ODBC.OdbcFunctions

> import Data.Char
> import Data.List
> import Data.Time
> import Database.Util
> import Test.MiniUnit
> import Foreign
> import Numeric


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
>     ++ "' union select '" ++ string2 ++ "' order by 1")
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 100
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
>   prepareStmt stmt "select 101"
>   executeStmt stmt
>   more <- fetch stmt
>   s <- getDataInt stmt 1
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchIntWithBuffer conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 101"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeInt 8
>   more <- fetch stmt
>   s <- getIntFromBuffer buffer
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchDouble conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select 123.456789"
>   executeStmt stmt
>   --buffer <- bindColBuffer stmt 1 sqlDTypeDouble 16
>   more <- fetch stmt
>   --s <- getDoubleFromBuffer buffer
>   s <- getDataDouble stmt 1
>   let expect :: Double; expect = 123.456789
>   assertEqual "testFetchDouble" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchDouble: EOD" (not more)
>   freeStmt stmt

> testFetchDatetime conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select timestamp without time zone '1916-10-01 02:25:21'"
>   --prepareStmt stmt "select timestamp without time zone '2007-10-01 02:25:21'"
>   executeStmt stmt
>   --buffer <- bindColBuffer stmt 1 sqlDTypeTimestamp 50
>   more <- fetch stmt
>   let expect :: UTCTime; expect = mkUTCTime 1916 10  1  2 25 21
>   --let expect :: UTCTime; expect = mkUTCTime 2007 10  1  2 25 21
>   --t <- getUtcTimeFromBindBuffer buffer
>   t <- getDataUtcTime stmt 1
>   assertEqual "testFetchDatetime" (Just expect) t
>   more <- fetch stmt
>   assertBool "testFetchDatetime: EOD" (not more)
>   freeStmt stmt

> testBindInt conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ?"
>   bindParamInt stmt 1 (Just 101)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeInt 8
>   more <- fetch stmt
>   s <- getIntFromBuffer buffer
>   let expect :: Int; expect = 101
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testBindString conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "set client_encoding to 'UTF8'"
>   executeStmt stmt
>   prepareStmt stmt "select ?"
>   let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   --let expect = "abc" ++ map chr [0x000078, 0x000079]
>   bindParamUTF8String stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 1000
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testBindString" (Just expect) s
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   freeStmt stmt

> testBindString2 conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "set client_encoding to 'UTF8'"
>   --prepareStmt stmt "set client_encoding to 'SQL_ASCII'"
>   executeStmt stmt
>   printIgnoreError (prepareStmt stmt "drop table t_utf8")
>   executeStmt stmt
>   prepareStmt stmt "create table t_utf8(s text)"
>   executeStmt stmt
>   --let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   --let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   let expect = "" ++ map chr [0x10FFFF]
>   prepareStmt stmt "insert into t_utf8 values ( ? )"
>   buffer <- bindParamUTF8String stmt 1 (Just expect)
>   printBufferContents buffer
>   executeStmt stmt
>   prepareStmt stmt "set client_encoding to 'SQL_ASCII'"
>   executeStmt stmt
>   prepareStmt stmt "select s from t_utf8"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 100
>   more <- fetch stmt
>   printBufferContents buffer
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testBindString2" (Just expect) s
>   freeStmt stmt

> printBufferContents (BindBuffer (bfptr, szfptr)) = do
>   withForeignPtr bfptr $ \bptr -> do
>   withForeignPtr szfptr $ \szptr -> do
>   sz <- peek szptr
>   putStrLn ("printBufferContents: sz = " ++ show sz)
>   --l <- peekArray (fromIntegral sz) (castPtr bptr)
>   l <- peekArray0 0 (castPtr bptr)
>   let toHex :: Word8 -> String; toHex i = showHex i ""
>   let h :: [String]; h = map toHex l
>   putStrLn (concat (intersperse " " h))

> testUTF8 conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "set client_encoding to 'UTF8'"
>   --prepareStmt stmt "set client_encoding to 'SQL_ASCII'"
>   executeStmt stmt
>   printIgnoreError (prepareStmt stmt "drop table t_utf8")
>   executeStmt stmt
>   prepareStmt stmt "create table t_utf8(s text)"
>   executeStmt stmt
>   --let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   --let expect = "abc" ++ map chr [0x000080, 0x0007FF, 0x00FFFF, 0x10FFFF]
>   --let expect = "" ++ map chr [0x10FFFF]
>   let expect = "" ++ map chr [0x0411]
>   prepareStmt stmt ("insert into t_utf8 values ( '" ++ expect ++ "' )")
>   executeStmt stmt
>   prepareStmt stmt "select s from t_utf8"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 100
>   more <- fetch stmt
>   printBufferContents buffer
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testUTF8" (Just expect) s
>   freeStmt stmt

> testBindUTCTime conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ?"
>   let expect :: UTCTime; expect = mkUTCTime 1916 10  1  2 25 21
>   bindParamUtcTime stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeTimestamp 50
>   more <- fetch stmt
>   t <- getUtcTimeFromBindBuffer buffer
>   assertEqual "testBindUTCTime" (Just expect) t
>   more <- fetch stmt
>   assertBool "testBindUTCTime: EOD" (not more)
>   freeStmt stmt

> testBindUTCTimeBoundary conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ?, ?"
>   let expect1 :: UTCTime; expect1 = mkUTCTime 1 1 1 0 0 0
>   let input1 :: UTCTime; input1 = mkUTCTime 0 1 1 0 0 0
>   let expect2 :: UTCTime; expect2 = mkUTCTime 9999 10  1  2 25 21
>   let input2 = expect2
>   bindParamUtcTime stmt 1 (Just input1)
>   bindParamUtcTime stmt 2 (Just input2)
>   executeStmt stmt
>   buffer1 <- bindColBuffer stmt 1 sqlDTypeTimestamp 32
>   buffer2 <- bindColBuffer stmt 2 sqlDTypeTimestamp 32
>   more <- fetch stmt
>   t1 <- getUtcTimeFromBindBuffer buffer1
>   t2 <- getUtcTimeFromBindBuffer buffer2
>   assertEqual "testBindUTCTimeBoundary1" (Just expect1) t1
>   assertEqual "testBindUTCTimeBoundary2" (Just expect2) t2
>   more <- fetch stmt
>   assertBool "testBindUTCTimeBoundary: EOD" (not more)
>   freeStmt stmt

> testRebind conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select ?"
>   --
>   let expect = "abc"
>   bindParamUTF8String stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 100
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testRebind1" (Just expect) s
>   --
>   closeCursor stmt
>   --
>   let expect = "xyz"
>   bindParamUTF8String stmt 1 (Just expect)
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 100
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testRebind2" (Just expect) s
>   --
>   more <- fetch stmt
>   assertBool "testBindString: EOD" (not more)
>   freeStmt stmt


> testlist =
>   testCreateStmt :
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
>   testCreateEnv
>   testCreateConn
>   testConnect connstr
>   (env, conn) <- createConn connstr
>   counts <- runTestTT "ODBC low-level tests" (mkTestlist conn testlist)
>   closeConn (env, conn)
>   return ()