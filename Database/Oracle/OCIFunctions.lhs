
|
Module      :  Database.Oracle.OCIFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple wrappers for OCI functions (FFI).
 
The functions in this file are simple wrappers for OCI functions.
The wrappers add error detection and exceptions;
functions in this module raise 'OCIException'.
The next layer up traps these and turns them into 'Database.Enumerator.DBException'.
 
Note that 'OCIException' /does not/ contain the error number and text
returned by 'getOCIErrorMsg'.
It is the job of the next layer (module) up to catch the 'OCIException'
and then call 'getOCIErrorMsg' to get the actual error details.
The 'OCIException' simply contains the error number returned by
the OCI call, and some text identifying the wrapper function.
See 'formatErrorCodeDesc' for the set of possible values for the OCI error numbers.

> {-# OPTIONS -ffi #-}
> {-# OPTIONS -fglasgow-exts #-}


> module Database.Oracle.OCIFunctions where


> import Database.Oracle.OCIConstants
> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic




|
 * Each handle type has its own data type, to prevent stupid errors
   i.e. using the wrong handle at the wrong time.
 
 * In GHC you can simply say @data OCIStruct@ i.e. there's no need for @= OCIStruct@.
   I've decided to be more portable, as it doesn't cost much.
 
 * Use castPtr if you need to convert handles (say 'OCIHandle' to a more specific type, or vice versa).

> data OCIStruct = OCIStruct
> type OCIHandle = Ptr OCIStruct  -- generic Handle for OCI functions that return Handles
> data OCIBuffer = OCIBuffer  -- generic buffer. Could hold anything: value or pointer.
> type BufferPtr = Ptr OCIBuffer
> type ColumnResultBuffer = ForeignPtr OCIBuffer  -- use ForeignPtr to ensure GC'd


> data EnvStruct = EnvStruct
> type EnvHandle = Ptr EnvStruct
> data ErrorStruct = ErrorStruct
> type ErrorHandle = Ptr ErrorStruct
> data ServerStruct = ServerStruct
> type ServerHandle = Ptr ServerStruct
> data UserStruct = UserStruct
> type UserHandle = Ptr UserStruct
> data ConnStruct = ConnStruct
> type ConnHandle = Ptr ConnStruct  -- AKA Service Context
> data SessStruct = SessStruct
> type SessHandle = Ptr SessStruct
> data StmtStruct = StmtStruct
> type StmtHandle = Ptr StmtStruct
> data DefnStruct = DefnStruct
> type DefnHandle = Ptr DefnStruct
> data ParamStruct = ParamStruct
> type ParamHandle = Ptr ParamStruct
> data BindStruct = BindStruct
> type BindHandle = Ptr BindStruct
> type ColumnInfo = (DefnHandle, ColumnResultBuffer, ForeignPtr CShort, ForeignPtr CShort)


|Low-level, OCI library errors.

> data OCIException = OCIException CInt String
>   deriving (Typeable, Show)

If we can't derive Typeable then the following code should do the trick:

 > data OCIException = OCIException CInt String
 > ociExceptionTc :: TyCon
 > ociExceptionTc = mkTyCon "Database.Oracle.OCIFunctions.OCIException"
 > instance Typeable OCIException where typeOf _ = mkAppTy ociExceptionTc []


> catchOCI :: IO a -> (OCIException -> IO a) -> IO a
> catchOCI = catchDyn

> throwOCI :: OCIException -> a
> throwOCI = throwDyn



> mkCInt :: Int -> CInt
> mkCInt n = fromIntegral n

> mkCShort :: CInt -> CShort
> mkCShort n = fromIntegral n

> cStrLen :: CStringLen -> CInt
> cStrLen = mkCInt . snd

> cStr :: CStringLen -> CString
> cStr = fst



---------------------------------------------------------------------------------
-- ** Foreign OCI functions
---------------------------------------------------------------------------------


> foreign import ccall "OCIEnvCreate" ociEnvCreate :: Ptr EnvHandle -> CInt -> Ptr a -> FunPtr a -> FunPtr a -> FunPtr a -> CInt -> Ptr (Ptr a) -> IO CInt
> foreign import ccall "OCIHandleAlloc" ociHandleAlloc :: OCIHandle -> Ptr OCIHandle -> CInt -> CInt -> Ptr a -> IO CInt
> foreign import ccall "oci.h OCIHandleFree" ociHandleFree :: OCIHandle -> CInt -> IO CInt
> foreign import ccall "oci.h OCIErrorGet" ociErrorGet :: OCIHandle -> CInt -> CString -> Ptr CInt -> CString -> CInt -> CInt -> IO CInt


> foreign import ccall "oci.h OCIParamGet" ociParamGet :: OCIHandle -> CInt -> ErrorHandle -> Ptr OCIHandle -> CInt -> IO CInt
> foreign import ccall "oci.h OCIAttrGet" ociAttrGet
>   :: OCIHandle -> CInt -> BufferPtr -> Ptr CInt -> CInt -> ErrorHandle -> IO CInt
> foreign import ccall "oci.h OCIAttrSet" ociAttrSet
>   :: OCIHandle -> CInt -> BufferPtr -> CInt -> CInt -> ErrorHandle -> IO CInt


> foreign import ccall "oci.h OCILogon" ociLogon
>   :: EnvHandle -> ErrorHandle -> Ptr ConnHandle -> CString -> CInt -> CString -> CInt -> CString -> CInt -> IO CInt
> foreign import ccall "oci.h OCILogoff" ociLogoff :: ConnHandle -> ErrorHandle -> IO CInt
> foreign import ccall "oci.h OCISessionBegin" ociSessionBegin :: ConnHandle -> ErrorHandle -> SessHandle -> CInt -> CInt -> IO CInt
> foreign import ccall "oci.h OCISessionEnd" ociSessionEnd :: ConnHandle -> ErrorHandle -> SessHandle -> CInt -> IO CInt
> foreign import ccall "oci.h OCIServerAttach" ociServerAttach :: ServerHandle -> ErrorHandle -> CString -> CInt -> CInt -> IO CInt
> foreign import ccall "oci.h OCIServerDetach" ociServerDetach :: ServerHandle -> ErrorHandle -> CInt -> IO CInt
> foreign import ccall "oci.h OCITerminate" ociTerminate :: CInt -> IO CInt

> foreign import ccall "oci.h OCITransStart" ociTransStart :: ConnHandle -> ErrorHandle -> Word8 -> CInt -> IO CInt
> foreign import ccall "oci.h OCITransCommit" ociTransCommit :: ConnHandle -> ErrorHandle -> CInt -> IO CInt
> foreign import ccall "oci.h OCITransRollback" ociTransRollback :: ConnHandle -> ErrorHandle -> CInt -> IO CInt

> foreign import ccall "oci.h OCIStmtPrepare" ociStmtPrepare :: StmtHandle -> ErrorHandle -> CString -> CInt -> CInt -> CInt -> IO CInt
> foreign import ccall "oci.h OCIDefineByPos" ociDefineByPos
>   :: StmtHandle -> Ptr DefnHandle -> ErrorHandle -> CInt -> BufferPtr -> CInt -> CShort -> Ptr CShort -> Ptr CShort -> Ptr CShort -> CInt -> IO CInt
> foreign import ccall "oci.h OCIStmtExecute" ociStmtExecute :: ConnHandle -> StmtHandle -> ErrorHandle -> CInt -> CInt -> OCIHandle -> OCIHandle -> CInt -> IO CInt
> foreign import ccall "oci.h OCIStmtFetch" ociStmtFetch :: StmtHandle -> ErrorHandle -> CInt -> CShort -> CInt -> IO CInt
 
stmt, ptr bindHdl, err, pos, valuePtr, sizeOfValue,
datatype, indicatorPtr, lenArrayPtr, retCodeArrayPtr,
plsqlArrayMaxLen, plsqlCurrEltPtr, mode

> foreign import ccall "oci.h OCIBindByPos" ociBindByPos ::
>   StmtHandle -> Ptr BindHandle -> ErrorHandle -> CUInt -> BufferPtr -> CInt
>   -> CUShort -> Ptr CShort -> Ptr CUShort -> Ptr CUShort
>   -> Ptr CUInt -> Ptr CUInt -> CUInt -> IO CInt


---------------------------------------------------------------------------------
-- ** OCI error reporting
---------------------------------------------------------------------------------

|This is just an auxiliary function for getOCIErrorMsg.

> getOCIErrorMsg2 :: OCIHandle -> CInt -> Ptr CInt -> CString -> CInt -> IO (CInt, String)
> getOCIErrorMsg2 ocihandle handleType errCodePtr errMsgBuf maxErrMsgLen = do
>   rc <- ociErrorGet ocihandle 1 nullPtr errCodePtr errMsgBuf maxErrMsgLen handleType
>   if rc < 0
>     then return (0, "Error message not available.")
>     else do
>       msg <- peekCString errMsgBuf
>       e <- peek errCodePtr
>       return (e, msg)


> getOCIErrorMsg :: OCIHandle -> CInt -> IO (CInt, String)
> getOCIErrorMsg ocihandle handleType = do
>   let stringBufferLen = 1000
>   allocaBytes stringBufferLen $ \errMsg ->
>     alloca $ \errCode ->
>     getOCIErrorMsg2 ocihandle handleType errCode errMsg (mkCInt stringBufferLen)

> fromEnumOCIErrorCode :: CInt -> String
> fromEnumOCIErrorCode err
>   | err == oci_SUCCESS = "OCI_SUCCESS"
>   | err == oci_SUCCESS_WITH_INFO = "OCI_SUCCESS_WITH_INFO"
>   | err == oci_NEED_DATA = "OCI_NEED_DATA"
>   | err == oci_NO_DATA = "OCI_NO_DATA"
>   | err == oci_INVALID_HANDLE = "OCI_INVALID_HANDLE"
>   | err == oci_STILL_EXECUTING = "OCI_STILL_EXECUTING"
>   | err == oci_CONTINUE = "OCI_CONTINUE"
>   | err == oci_RESERVED_FOR_INT_USE = "OCI_RESERVED_FOR_INT_USE"
>   | otherwise = "OCI_ERROR"

> formatErrorCodeDesc :: CInt -> String -> String
> formatErrorCodeDesc err desc
>   | err == oci_ERROR = ""
>   | otherwise = (fromEnumOCIErrorCode err) ++ " - " ++ desc


|Given the two parts of an 'OCIException' (the error number and text)
get the actual error message from the DBMS and construct an error message
from all of these pieces.

> formatOCIMsg :: CInt -> String -> OCIHandle -> CInt -> IO (Int, String)
> formatOCIMsg e m ocihandle handleType = do
>   (err, msg) <- getOCIErrorMsg ocihandle handleType
>   --return (fromIntegral err, (formatErrorCodeDesc e m) ++ " : " ++ (show err) ++ " - " ++ msg)
>   if msg == ""
>     then return (fromIntegral err, (formatErrorCodeDesc e m))
>     else return (fromIntegral err, (formatErrorCodeDesc e m) ++ " : " ++ msg)



|We have two format functions: 'formatEnvMsg' takes the 'EnvHandle',
'formatErrorMsg' takes the 'ErrorHandle'.
They're just type-safe wrappers for 'formatMsgCommon'.

> formatMsgCommon :: OCIException -> OCIHandle -> CInt -> IO (Int, String)
> formatMsgCommon (OCIException e m) h handleType = do
>   if e == 0
>     then return (0, "")
>     else case () of
>       _ | e == oci_ERROR -> do (formatOCIMsg e m h handleType)
>         | e == oci_SUCCESS_WITH_INFO -> do (formatOCIMsg e m h handleType)
>         | otherwise -> return (fromIntegral e, formatErrorCodeDesc e m)

> formatErrorMsg :: OCIException -> ErrorHandle -> IO (Int, String)
> formatErrorMsg exc err = formatMsgCommon exc (castPtr err) oci_HTYPE_ERROR

> formatEnvMsg :: OCIException -> EnvHandle -> IO (Int, String)
> formatEnvMsg exc err = formatMsgCommon exc (castPtr err) oci_HTYPE_ENV



|The testForError functions are the only places where OCIException is thrown,
so if you want to change or embellish it, your changes will be localised here.
These functions factor out common error handling code
from the OCI wrapper functions that follow.
 
Typically an OCI wrapper function would look like:
 
 > handleAlloc handleType env = alloca ptr -> do
 >   rc <- ociHandleAlloc env ptr handleType 0 nullPtr
 >   if rc < 0
 >     then throwOCI (OCIException rc msg)
 >     else return ()
 
where the code from @if rc < 0@ onwards was identical.
'testForError' replaces the code from @if rc < 0 ...@ onwards.

> testForError :: CInt -> String -> a -> IO a
> testForError rc msg retval = do
>   if rc < 0
>     then throwOCI (OCIException rc msg)
>     else return retval


|Like 'testForError' but when the value you want to return
is at the end of a pointer.
Either there was an error, in which case the pointer probably isn't valid,
or there is something at the end of the pointer to return.
See 'dbLogon' and 'getHandleAttr' for example usage.

> testForErrorWithPtr :: Storable a => CInt -> String -> Ptr a -> IO a
> testForErrorWithPtr rc msg retval = do
>   if rc < 0
>     then throwOCI (OCIException rc msg)
>     else peek retval


---------------------------------------------------------------------------------
-- ** Allocating Handles (i.e. creating OCI data structures, and memory management)
---------------------------------------------------------------------------------


> envCreate :: IO EnvHandle
> envCreate = alloca $ \ptr -> do
>   rc <- ociEnvCreate ptr oci_DEFAULT nullPtr nullFunPtr nullFunPtr nullFunPtr 0 nullPtr
>   testForErrorWithPtr rc "allocate initial end" ptr

> handleAlloc :: CInt -> OCIHandle -> IO OCIHandle
> handleAlloc handleType env = alloca $ \ptr -> do
>   rc <- ociHandleAlloc env ptr handleType 0 nullPtr
>   testForErrorWithPtr rc "allocate handle" ptr

> handleFree :: CInt -> OCIHandle -> IO ()
> handleFree handleType ptr = do
>    rc <- ociHandleFree ptr handleType
>    testForError rc "free handle" ()



> setHandleAttr :: ErrorHandle -> OCIHandle -> CInt -> Ptr a -> CInt -> IO ()
> setHandleAttr err ocihandle handleType handleAttr attrType = do
>   rc <- ociAttrSet ocihandle handleType (castPtr handleAttr) 0 attrType err
>   testForError rc "setHandleAttr" ()


> setHandleAttrString :: ErrorHandle -> OCIHandle -> CInt -> String -> CInt -> IO ()
> setHandleAttrString err ocihandle handleType s attrType = do
>   withCStringLen s $ \sC -> do
>     rc <- ociAttrSet ocihandle handleType (castPtr (cStr sC)) (cStrLen sC) attrType err
>     testForError rc "setHandleAttrString" ()


ociAttrGet returns a pointer to something - maybe a handle or a chunk of memory.
Sometimes it's a pointer to a Handle, i.e. a Ptr to a Ptr to a Struct,
so we want to peek it to get the Handle.
Other times it's a pointer to (say) a few bytes which might contain a number or a string.
Deref'ing it returns that value immediately, rather than a Ptr to that value.

> getHandleAttr :: (Storable a) => ErrorHandle -> OCIHandle -> CInt -> CInt -> IO a
> getHandleAttr err ocihandle handleType attrType = alloca $ \ptr -> do
>   -- 3rd arg has type Ptr OCIBuffer.
>   rc <- ociAttrGet ocihandle handleType (castPtr ptr) nullPtr attrType err
>   testForErrorWithPtr rc "getAttrHandle" ptr

> getParam :: ErrorHandle -> StmtHandle -> Int -> IO ParamHandle
> getParam err stmt posn = alloca $ \ptr -> do
>   rc <- ociParamGet (castPtr stmt) oci_HTYPE_STMT err ptr (mkCInt posn)
>   testForErrorWithPtr rc "getParam" (castPtr ptr)
     

---------------------------------------------------------------------------------
-- ** Connecting and detaching
---------------------------------------------------------------------------------

|The OCI Logon function doesn't behave as you'd expect when the password is due to expire.
'ociLogon' returns 'Database.Oracle.OCIConstants.oci_SUCCESS_WITH_INFO',
but the 'ConnHandle' returned is not valid.
In this case we have to change 'Database.Oracle.OCIConstants.oci_SUCCESS_WITH_INFO'
to 'Database.Oracle.OCIConstants.oci_ERROR',
so that the error handling code will catch it and abort. 
I don't know why the handle returned isn't valid,
as the logon process should be able to complete successfully in this case.


> dbLogon :: String -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
> dbLogon user pswd db env err =
>   withCStringLen user $ \userC ->
>   withCStringLen pswd $ \pswdC ->
>   withCStringLen db   $ \dbC ->
>   alloca $ \conn -> do
>     rc <- ociLogon env err conn (cStr userC) (cStrLen userC) (cStr pswdC) (cStrLen pswdC) (cStr dbC) (cStrLen dbC)
>     case () of
>       _ | rc == oci_SUCCESS_WITH_INFO -> testForErrorWithPtr oci_ERROR "logon" conn
>         | otherwise -> testForErrorWithPtr rc "logon" conn


> dbLogoff :: ErrorHandle -> ConnHandle -> IO ()
> dbLogoff err conn = do
>   rc <- ociLogoff conn err
>   testForError rc "logoff" ()


> terminate :: IO ()
> terminate = do
>   rc <- ociTerminate oci_DEFAULT
>   testForError rc "terminate" ()



> serverDetach :: ErrorHandle -> ServerHandle -> IO ()
> serverDetach err server = do
>   rc <- ociServerDetach server err oci_DEFAULT
>   testForError rc "server detach" ()


> serverAttach :: ErrorHandle -> ServerHandle -> String -> IO ()
> serverAttach err server dblink = do
>   withCStringLen dblink $ \s -> do
>     rc <- ociServerAttach server err (cStr s) (cStrLen s) oci_DEFAULT
>     testForError rc "server attach" ()


|Having established a connection (Service Context), now get the Session.
You can have more than one session per connection,
but I haven't implemented it yet.

> getSession :: ErrorHandle -> ConnHandle -> IO SessHandle
> getSession err conn = liftM castPtr (getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION)


> sessionBegin :: ErrorHandle -> ConnHandle -> SessHandle -> CInt -> IO ()
> sessionBegin err conn sess cred = do
>   rc <- ociSessionBegin conn err sess cred oci_DEFAULT
>   testForError rc "session begin" ()


> sessionEnd :: ErrorHandle -> ConnHandle -> SessHandle -> IO ()
> sessionEnd err conn sess = do
>   rc <- ociSessionEnd conn err sess oci_DEFAULT
>   testForError rc "session end" ()



---------------------------------------------------------------------------------
-- ** Transactions
---------------------------------------------------------------------------------

> beginTrans :: ErrorHandle -> ConnHandle -> CInt -> IO ()
> beginTrans err conn isolation = do
>   rc <- ociTransStart conn err 0 isolation
>   testForError rc "begin transaction" ()

> commitTrans :: ErrorHandle -> ConnHandle -> IO ()
> commitTrans err conn = do
>   rc <- ociTransCommit conn err oci_DEFAULT
>   testForError rc "commit" ()

> rollbackTrans :: ErrorHandle -> ConnHandle -> IO ()
> rollbackTrans err conn = do
>   rc <- ociTransRollback conn err oci_DEFAULT
>   testForError rc "rollback" ()


---------------------------------------------------------------------------------
-- ** Issuing queries
---------------------------------------------------------------------------------

|With the OCI you do queries with these steps:
 
 * prepare your statement (it's just a String) - no communication with DBMS
 
 * execute it (this sends it to the DBMS for parsing etc)
 
 * allocate result set buffers by calling 'defineByPos' for each column
 
 * call fetch for each row.
 
 * call 'handleFree' for the 'StmtHandle'
   (I assume this is the approved way of terminating the query;
   the OCI docs aren't explicit about this.)


> stmtPrepare :: ErrorHandle -> StmtHandle -> String -> IO ()
> stmtPrepare err stmt sqltext = do
>   withCStringLen sqltext $ \sqltextC -> do
>     rc <- ociStmtPrepare stmt err (cStr sqltextC) (cStrLen sqltextC) oci_NTV_SYNTAX oci_DEFAULT
>     testForError rc "stmtPrepare" ()


> stmtExecute :: ErrorHandle -> ConnHandle -> StmtHandle -> Int -> IO ()
> stmtExecute err conn stmt iterations = do
>   rc <- ociStmtExecute conn stmt err (mkCInt iterations) 0 nullPtr nullPtr oci_DEFAULT
>   testForError rc "stmtExecute" ()



|defineByPos allocates memory for a single column value.
The allocated components are:
 
 * the result (i.e. value) - you have to say how big with bufsize.
 
 * the null indicator (int16)
 
 * the size of the returned data (int16)
 
Previously it was the caller's responsibility to free the memory after they're done with it.
Now we use 'Foreign.ForeignPtr.mallocForeignPtr', so manual memory management is hopefully
a thing of the past.
The caller will also have to cast the data in bufferptr to the expected type
(using 'Foreign.Ptr.castPtr').


> defineByPos :: ErrorHandle
>   -> StmtHandle
>   -> Int   -- ^ Position
>   -> Int   -- ^ Buffer size in bytes
>   -> CInt  -- ^ SQL Datatype (from "Database.Oracle.OCIConstants")
>   -> IO ColumnInfo  -- ^ tuple: (DefnHandle, Ptr to buffer, Ptr to null indicator, Ptr to size of value in buffer)
> defineByPos err stmt posn bufsize sqldatatype = do
>   bufferFPtr <- mallocForeignPtrBytes bufsize
>   nullIndFPtr <- mallocForeignPtr
>   retSizeFPtr <- mallocForeignPtr
>   alloca $ \defnPtr ->
>     withForeignPtr bufferFPtr $ \bufferPtr ->
>     withForeignPtr nullIndFPtr $ \nullIndPtr ->
>     withForeignPtr retSizeFPtr $ \retSizePtr -> do
>     rc <- ociDefineByPos stmt defnPtr err (mkCInt posn) bufferPtr (mkCInt bufsize) (mkCShort sqldatatype) nullIndPtr retSizePtr nullPtr oci_DEFAULT
>     defn <- peek defnPtr  -- no need for caller to free defn; I think freeing the stmt handle does it.
>     testForError rc "defineByPos" (defn, bufferFPtr, nullIndFPtr, retSizeFPtr)


|Oracle only understands bind variable placeholders using syntax :x,
where x is a number or a variable name.
Most other DBMS's use ? as a placeholder,
so we have this function to substitute ? with :n,
where n starts at one and increases with each ?.
 
We don't hook this in function into this library though;
it's used in the higher-level implementation of Enumerator.
I like to retain flexibility at this lower-level,
and not force arbitrary implementation choices too soon.
If you want to use this library and use :x style syntax, you can.

> substituteBindPlaceHolders sql =
>   sbph sql 1 False ""

> sbph :: String -> Int -> Bool -> String -> String
> sbph [] _ _ acc = reverse acc
> sbph ('\'':cs) i inQuote acc = sbph cs i (not inQuote) ('\'':acc)
> sbph ('?':cs) i False acc = sbph cs (i+1) False ((reverse (show i)) ++ (':':acc))
> sbph (c:cs) i inQuote acc = sbph cs i inQuote (c:acc)



> bindByPos ::
>   ErrorHandle
>   -> StmtHandle
>   -> Int   -- ^ Position
>   -> CShort   -- ^ Null ind: 0 == not null, -1 == null
>   -> BufferPtr  -- ^ payload
>   -> Int   -- ^ payload size in bytes
>   -> CInt  -- ^ SQL Datatype (from "Database.Oracle.OCIConstants")
>   -> IO ()
> bindByPos err stmt pos nullInd bufptr sze sqltype =
>   alloca $ \bindHdl ->
>   alloca $ \indPtr -> do
>     poke indPtr nullInd
>     -- we don't use the bind handle returned; I think it's freed when the stmt is.
>     rc <- ociBindByPos stmt bindHdl err (fromIntegral pos) bufptr
>             (fromIntegral sze) (fromIntegral sqltype)
>             indPtr nullPtr nullPtr nullPtr nullPtr (fromIntegral oci_DEFAULT)
>     testForError rc "bindByPos" ()


|stmtFetch takes a lot of run-time
because it involves a network trip to the DBMS for each call.

> stmtFetch :: ErrorHandle -> StmtHandle -> IO CInt
> stmtFetch err stmt = do
>   let numRowsToFetch = 1
>   rc <- ociStmtFetch stmt err numRowsToFetch (mkCShort oci_FETCH_NEXT) oci_DEFAULT
>   if rc == oci_NO_DATA
>     then return rc
>     else testForError rc "stmtFetch" rc
