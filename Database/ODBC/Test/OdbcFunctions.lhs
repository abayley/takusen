
> module Database.ODBC.Test.OdbcFunctions where

> import Database.ODBC.OdbcFunctions

> import Data.Char
> import Data.Time
> import Database.Util
> import Test.MiniUnit


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

> testConnect dsn = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   connstr <- connect conn ("DSN=" ++ dsn)
>   disconnect conn
>   freeConn conn
>   freeEnv env

> createConn dsn = do
>   env <- allocEnv
>   setOdbcVer env
>   conn <- allocConn env
>   connstr <- connect conn ("DSN=" ++ dsn)
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
>   buffer <- bindColBuffer stmt 1 sqlDTypeString 100
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testFetchString" (Just string1) s
>   more <- fetch stmt
>   s <- getUTF8StringFromBuffer buffer
>   assertEqual "testFetchString" (Just string2) s
>   more <- fetch stmt
>   assertBool "testFetchString: EOD" (not more)
>   freeStmt stmt

> testFetchInt conn = do
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
>   buffer <- bindColBuffer stmt 1 sqlDTypeDouble 16
>   more <- fetch stmt
>   s <- getDoubleFromBuffer buffer
>   let expect :: Double; expect = 123.456789
>   assertEqual "testFetchInt" (Just expect) s
>   more <- fetch stmt
>   assertBool "testFetchInt: EOD" (not more)
>   freeStmt stmt

> testFetchDatetime conn = do
>   stmt <- allocStmt conn
>   prepareStmt stmt "select timestamp without time zone '1916-10-01 02:25:21'"
>   --prepareStmt stmt "select timestamp without time zone '2007-10-01 02:25:21'"
>   executeStmt stmt
>   buffer <- bindColBuffer stmt 1 sqlDTypeTimestamp 50
>   more <- fetch stmt
>   let expect :: UTCTime; expect = mkUTCTime 1916 10  1  2 25 21
>   --let expect :: UTCTime; expect = mkUTCTime 2007 10  1  2 25 21
>   t <- getUtcTimeFromBuffer buffer
>   assertEqual "testFetchDatetime" (Just expect) t
>   more <- fetch stmt
>   assertBool "testFetchDatetime: EOD" (not more)
>   freeStmt stmt

> testlist =
>   testCreateStmt :
>   testFetchString :
>   testFetchInt :
>   testFetchDouble :
>   testFetchDatetime :
>   []

> mkTestlist conn testlist = map (\testcase -> printIgnoreError (testcase conn)) testlist

> parseArgs :: [String] -> IO String
> parseArgs args = do
>    let (dsn:_) = args
>    return dsn

> runTest :: [String] -> IO ()
> runTest as = do
>   dsn <- parseArgs as
>   testCreateEnv
>   testCreateConn
>   testConnect dsn
>   (env, conn) <- createConn dsn
>   counts <- runTestTT "ODBC low-level tests" (mkTestlist conn testlist)
>   closeConn (env, conn)
>   return ()
