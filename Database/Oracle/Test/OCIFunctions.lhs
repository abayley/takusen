
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

> module Database.Oracle.Test.OCIFunctions (runTest) where

> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions (EnvHandle, ErrorHandle, ConnHandle, StmtHandle)
> import Database.Oracle.OCIConstants
> import Foreign.Ptr
> import Foreign.C.String
> import Foreign.C.Types
> import Foreign
> import System.IO
> import System.Environment (getArgs)
> import Control.Monad


> nullAction :: IO ()
> nullAction = return ()

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

> reportAndIgnore :: ErrorHandle -> OCI.OCIException -> IO () -> IO ()
> reportAndIgnore err ociexc cleanupAction = do
>   (e, m) <- OCI.formatErrorMsg ociexc err
>   printError $ (show e) ++ ": " ++ m
>   cleanupAction

> reportAndRethrow :: ErrorHandle -> OCI.OCIException -> IO () -> IO ()
> reportAndRethrow err ociexc cleanupAction = do
>   (_, m) <- OCI.formatErrorMsg ociexc err
>   printError m
>   cleanupAction
>   OCI.throwOCI ociexc


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
>     -- and we need to create a valid transaction handle for the connection, too.
>     trans <- OCI.handleAlloc oci_HTYPE_TRANS (castPtr env)
>     OCI.setHandleAttr (castPtr err) (castPtr conn) oci_HTYPE_SVCCTX (castPtr trans) oci_ATTR_TRANS
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


> testBeginTrans :: (String, String, String) -> IO ()
> testBeginTrans args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       --OCI.beginTrans err conn oci_TRANS_SERIALIZABLE 
>       OCI.beginTrans err conn 1
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   stmt <- getStmt env err conn "select dummy from dual"
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)


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


> testBind :: (String, String, String) -> IO ()
> testBind args = do
>   (env, err, conn) <- logon args
>   stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>   OCI.stmtPrepare err (castPtr stmt) "select :1 from dual union select :2 from dual"
>   withCStringLen "hello" $ \(cstr, clen) ->
>     OCI.bindByPos err (castPtr stmt) 1 0 (castPtr cstr) clen oci_SQLT_CHR
>   withCStringLen "hello2" $ \(cstr, clen) ->
>     OCI.bindByPos err (castPtr stmt) 2 0 (castPtr cstr) clen oci_SQLT_CHR
>   OCI.stmtExecute err conn (castPtr stmt) 0
>   buffer@(_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   s <- bufferToString buffer
>   putStrLn $ "bind: fetched: " ++ s
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   s <- bufferToString buffer
>   putStrLn $ "bind: fetched: " ++ s
>   rc <- OCI.stmtFetch err (castPtr stmt)
>   OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>   logoff (env, err, conn)

> cShort2Int :: CShort -> Int
> cShort2Int n = fromIntegral n
> nullByte :: CChar
> nullByte = 0

> --bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString (_, bufFPtr, nullFPtr, sizeFPtr) =
>   -- If it's null then return ""
>   withForeignPtr nullFPtr $ \nullIndPtr -> do
>     nullInd <- liftM cShort2Int (peek nullIndPtr)
>     if (nullInd == -1)  -- -1 == null, 0 == value
>       then return ""
>       else do
>         -- Given a column buffer, extract a string of variable length
>         -- (you have to terminate it yourself).
>         withForeignPtr bufFPtr $ \bufferPtr ->
>           withForeignPtr sizeFPtr $ \retSizePtr -> do
>             retsize <- liftM cShort2Int (peek retSizePtr)
>             pokeByteOff (castPtr bufferPtr) retsize nullByte
>             val <- peekCString (castPtr bufferPtr)
>             return val


> parseArgs :: IO (String, String, String)
> parseArgs = do
>    [u, p, d] <- getArgs
>    return (u, p, d)


> runTest :: IO ()
> runTest = do
>   args <- parseArgs
>   testCreateEnv
>   testConnect args
>   testBeginTrans args
>   testExecute args
>   testFetch args
>   testFetchFail args
>   testBind args
