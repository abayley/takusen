
|
Module      :  Database.Oracle.Test.OCIFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Test harness for "Database.Oracle.OCIFunctions".
This module depends on on "Database.Oracle.OCIFunctions".
so it must only use functions from there.


> {-# OPTIONS -fglasgow-exts #-}

> module Database.Oracle.Test.OCIFunctions (runOCITest) where

> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions (EnvHandle, ErrorHandle, ConnHandle, StmtHandle)
> import Database.Oracle.OCIConstants
> import Foreign.Ptr


> testUser :: String
> testUser = "mama"

> testPswd :: String
> testPswd = "mama"

> testDb :: String
> testDb = "chronicle_dev"


> logon :: IO (EnvHandle, ErrorHandle, ConnHandle)
> logon = do
>   env <- OCI.envCreate
>   err <- OCI.handleAlloc oci_HTYPE_ERROR (castPtr env)
>   conn <- OCI.dbLogon testUser testPswd testDb env (castPtr err)
>   return (env, castPtr err, conn)

> logoff :: (EnvHandle, ErrorHandle, ConnHandle) -> IO ()
> logoff (env, err, conn) = do
>   OCI.dbLogoff err conn
>   OCI.handleFree oci_HTYPE_ERROR (castPtr err)
>   OCI.handleFree oci_HTYPE_ENV (castPtr env)
>   OCI.terminate


> testCreateEnv :: IO ()
> testCreateEnv = do
>   putStrLn "testCreateEnv start"
>   env <- OCI.envCreate
>   OCI.handleFree oci_HTYPE_ENV (castPtr env)
>   putStrLn "testCreateEnv done"


> testConnect :: IO ()
> testConnect = do
>   putStrLn "testConnect start"
>   x <- logon
>   logoff x
>   putStrLn "testConnect done"


> getStmt :: EnvHandle -> ErrorHandle -> ConnHandle -> String -> IO StmtHandle
> getStmt env err conn sql = do
>   putStrLn "getStmt start"
>   stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>   putStrLn "getStmt prepare"
>   OCI.stmtPrepare err (castPtr stmt) sql
>   putStrLn "getStmt execute"
>   OCI.stmtExecute err conn (castPtr stmt) 0
>   putStrLn "getStmt return"
>   return (castPtr stmt)


> testExecute :: IO ()
> testExecute = do
>   (env, err, conn) <- logon
>   stmt <- getStmt env err conn "select dummy from dual"
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


> testFetch :: IO ()
> testFetch = do
>   (env, err, conn) <- logon
>   stmt <- getStmt env err conn "select dummy from dual"
>   (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)



> testFetchFail :: IO ()
> testFetchFail = do
>   (env, err, conn) <- logon
>   stmt <- getStmt env err conn "select dummy, 1 from dual"
>   (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


> runOCITest :: IO ()
> runOCITest = do
>   testCreateEnv
>   testConnect
>   testExecute
>   testFetch
>   testFetchFail
