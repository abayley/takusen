
|
Module      :  Database.Oracle.Test.OCIFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Test harness for "Database.Oracle.OCIFunctions".
This module depends on on "Database.Oracle.OCIFunctions".
so it should only use functions from there (and "Database.Oracle.OCIConstants").


> {-# OPTIONS -fglasgow-exts #-}

> module Database.Oracle.Test.OCIFunctions (runOCITest) where

> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions (EnvHandle, ErrorHandle, ConnHandle, StmtHandle)
> import Database.Oracle.OCIConstants
> import Foreign.Ptr
> import System.IO
> import System.Environment (getArgs)


> nullAction :: IO ()
> nullAction = return ()

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

> reportAndIgnore :: ErrorHandle -> OCI.OCIException -> IO () -> IO ()
> reportAndIgnore err ociexc cleanupAction = do
>   (_, m) <- OCI.formatErrorMsg ociexc err
>   printError m
>   cleanupAction


> logon :: (String, String, String) -> IO (EnvHandle, ErrorHandle, ConnHandle)
> logon (testUser, testPswd, testDb) = do
>   env <- OCI.envCreate
>   err <- OCI.handleAlloc oci_HTYPE_ERROR (castPtr env)
>   OCI.catchOCI ( do
>     server <- OCI.handleAlloc oci_HTYPE_SERVER (castPtr env)
>     OCI.serverAttach (castPtr err) (castPtr server) testDb
>     conn <- OCI.handleAlloc oci_HTYPE_SVCCTX (castPtr env)
>     -- the connection holds a reference to the server in one of its attributes
>     OCI.setHandleAttr (castPtr err) (castPtr conn) oci_HTYPE_SVCCTX (castPtr server) oci_ATTR_SERVER
>     session <- OCI.handleAlloc oci_HTYPE_SESSION (castPtr env)
>     if (testUser == "")
>       then do
>         OCI.sessionBegin (castPtr err) (castPtr conn) (castPtr session) oci_CRED_EXT
>       else do
>         OCI.setHandleAttrString (castPtr err) (castPtr session) oci_HTYPE_SESSION testUser oci_ATTR_USERNAME
>         OCI.setHandleAttrString (castPtr err) (castPtr session) oci_HTYPE_SESSION testPswd oci_ATTR_PASSWORD
>         OCI.sessionBegin (castPtr err) (castPtr conn) (castPtr session) oci_CRED_RDBMS
>     -- the connection also holds a reference to the session in one of its attributes
>     OCI.setHandleAttr (castPtr err) (castPtr conn) oci_HTYPE_SVCCTX (castPtr session) oci_ATTR_SESSION
>     return (env, castPtr err, castPtr conn)
>     ) (\ociexc -> do
>       reportAndIgnore (castPtr err) ociexc nullAction
>       return (nullPtr, nullPtr, nullPtr)
>     )

> logoff :: (EnvHandle, ErrorHandle, ConnHandle) -> IO ()
> logoff (env, err, conn) = OCI.catchOCI ( do
>     session <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION
>     server <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SERVER
>     OCI.sessionEnd err conn session
>     OCI.serverDetach err server
>     OCI.handleFree oci_HTYPE_SESSION (castPtr session)
>     OCI.handleFree oci_HTYPE_SERVER (castPtr server)
>     OCI.handleFree oci_HTYPE_SVCCTX (castPtr conn)
>     OCI.handleFree oci_HTYPE_ERROR (castPtr err)
>     OCI.handleFree oci_HTYPE_ENV (castPtr env)
>     OCI.terminate
>   ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)


> testCreateEnv :: IO ()
> testCreateEnv = do
>   putStrLn "testCreateEnv start"
>   env <- OCI.envCreate
>   OCI.handleFree oci_HTYPE_ENV (castPtr env)
>   putStrLn "testCreateEnv done"


> testConnect :: (String, String, String) -> IO ()
> testConnect args = do
>   putStrLn "testConnect start"
>   x <- logon args
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


> testExecute :: (String, String, String) -> IO ()
> testExecute args = do
>   (env, err, conn) <- logon args
>   stmt <- getStmt env err conn "select dummy from dual"
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


> testFetch :: (String, String, String) -> IO ()
> testFetch args = do
>   (env, err, conn) <- logon args
>   stmt <- getStmt env err conn "select dummy from dual"
>   (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)



> testFetchFail :: (String, String, String) -> IO ()
> testFetchFail args = do
>   (env, err, conn) <- logon args
>   stmt <- getStmt env err conn "select dummy, 1 from dual"
>   (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


> parseArgs :: IO (String, String, String)
> parseArgs = do
>    [u, p, d] <- getArgs
>    return (u, p, d)


> runOCITest :: IO ()
> runOCITest = do
>   args <- parseArgs
>   testCreateEnv
>   testConnect args
>   testExecute args
>   testFetch args
>   testFetchFail args
