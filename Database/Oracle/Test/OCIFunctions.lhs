
|
Module      :  Database.Oracle.Test.OCIFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Test harness for "Database.Oracle.OCIFunctions".


> {-# OPTIONS -ffi -fglasgow-exts #-}

> module Database.Oracle.Test.OCIFunctions where

> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions (EnvHandle, ErrorHandle, ConnHandle, StmtHandle)
> import Database.Oracle.OCIConstants
> import Foreign.Ptr
> import Foreign.Marshal


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


> testConnect :: IO ()
> testConnect = do
>   x <- logon
>   logoff x


> getStmt :: EnvHandle -> ErrorHandle -> ConnHandle -> String -> IO StmtHandle
> getStmt env err conn sql = do
>   stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>   OCI.stmtPrepare err (castPtr stmt) sql
>   OCI.stmtExecute err conn (castPtr stmt) 0
>   return (castPtr stmt)

> testExecute = do
>   (env, err, conn) <- logon
>   stmt <- getStmt env err conn "select dummy from dual"
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


> testFetch = do
>   (env, err, conn) <- logon
>   stmt <- getStmt env err conn "select dummy from dual"
>   (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   free buf
>   free nullptr
>   free sizeptr
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)



> testFetchFail = do
>   (env, err, conn) <- logon
>   stmt <- getStmt env err conn "select dummy, 1 from dual"
>   (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   free buf
>   free nullptr
>   free sizeptr
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


> allTests :: IO ()
> allTests = do
>   testConnect
>   testExecute
