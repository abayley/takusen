
|
Module      :  Database.Enumerator.OCIEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Oracle OCI implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}

> module Database.Oracle.OCIEnumerator where


> import Database.Enumerator
> import Database.Oracle.OCIConstants
> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions
>   ( OCIHandle, EnvHandle, ErrorHandle, ServerHandle, ConnHandle, SessHandle, StmtHandle
>   , OCIException (..), catchOCI)
> import Foreign
> import Foreign.C
> import Foreign.C.Types
> import Foreign.Marshal
> import Foreign.Marshal.Utils (with)
> import Control.Monad
> import Control.Monad.State
> import Control.Exception (throwIO)
> import System.Time
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.IORef
> import Data.Word
> import System.IO (hPutStrLn, stderr)


--------------------------------------------------------------------
-- ** Error handling
--------------------------------------------------------------------



> nullAction :: IO ()
> nullAction = return ()

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

|convertAndRethrow converts an OCIException to a DBException.
The third parameter is an IO action that you can use to clean up any handles.
First we get the error message from the ErrorHandle,
and then we run the cleanup action to free any allocated handles.
(Obviously, we must extract the error message _before_ we free the handles.)
If there's no cleanup action required then simply pass nullAction.

> convertAndRethrow :: ErrorHandle -> OCIException -> IO () -> IO ()
> convertAndRethrow err ociexc cleanupAction = do
>   (e, m) <- OCI.formatErrorMsg ociexc err
>   cleanupAction
>   throwDB (DBError e m)

> reportAndIgnore :: ErrorHandle -> OCIException -> IO () -> IO ()
> reportAndIgnore err ociexc cleanupAction = do
>   (_, m) <- OCI.formatErrorMsg ociexc err
>   printError m
>   cleanupAction

> reportAndRethrow :: ErrorHandle -> OCIException -> IO () -> IO ()
> reportAndRethrow err ociexc cleanupAction = do
>   (_, m) <- OCI.formatErrorMsg ociexc err
>   printError m
>   OCI.throwOCI ociexc


|What do we do if creating the first handle (Environment) fails?
There's no Env- or ErrorHandle to get the error message from,
so do the best we can by constructing a message with formatErrorCodeDesc.
Still throws DBException.

> reportOCIExc :: OCIException -> IO ()
> reportOCIExc (OCIException e m) = do
>   let s = OCI.formatErrorCodeDesc e m
>   printError s
>   throwDB (DBError 0 s)


|This is a version of convertAndRethrow version that uses EnvHandle
rather than ErrorHandle. Only used by getErr.

> convertAndRethrowEnv :: EnvHandle -> OCIException -> IO () -> IO ()
> convertAndRethrowEnv env ociexc cleanupAction = do
>   (e, m) <- OCI.formatEnvMsg ociexc env
>   cleanupAction
>   throwDB (DBError e m)


--------------------------------------------------------------------
-- ** OCI Function Wrappers
--------------------------------------------------------------------


> data Session = Session 
>   { envHandle :: EnvHandle
>   , errorHandle :: ErrorHandle
>   , connHandle :: ConnHandle
>   }

> data Query = Query
>   { stmtHandle :: StmtHandle
>   , queryResourceUsage :: QueryResourceUsage
>   }


|Reports and ignores any errors when freeing handles.
Will catch attempts to free invalid (already freed?) handles.

> freeHandle :: OCIHandle -> CInt -> IO ()
> freeHandle ocihandle handleType = catchOCI ( do
>     OCI.handleFree handleType ocihandle
>   ) (\(OCIException e m) -> do
>     let s = OCI.formatErrorCodeDesc e m
>     printError s
>   )


> getEnv :: IO EnvHandle
> getEnv = catchOCI ( do
>   OCI.envCreate
>   ) (\ociexc -> do
>     reportOCIExc ociexc
>     return nullPtr
>   )

> getErr :: EnvHandle -> IO ErrorHandle
> getErr env = catchOCI ( do
>     err <- OCI.handleAlloc oci_HTYPE_ERROR (castPtr env)
>     return (castPtr err)
>   ) (\ociexc -> do
>     convertAndRethrowEnv env ociexc (freeHandle (castPtr env) oci_HTYPE_ENV)
>     return nullPtr
>   )


> getServer :: EnvHandle -> ErrorHandle -> IO ServerHandle
> getServer env err = catchOCI ( do
>     server <- OCI.handleAlloc oci_HTYPE_SERVER (castPtr env)
>     return (castPtr server)
>   ) (\ociexc -> do
>     convertAndRethrow err ociexc $ do
>       freeHandle (castPtr err) oci_HTYPE_ERROR
>       freeHandle (castPtr env) oci_HTYPE_ENV
>     return nullPtr
>   )


> getConnection :: EnvHandle -> ErrorHandle -> IO ConnHandle
> getConnection env err = catchOCI ( do
>     conn <- OCI.handleAlloc oci_HTYPE_SVCCTX (castPtr env)
>     return (castPtr conn)
>   ) (\ociexc -> do
>     convertAndRethrow err ociexc $ do
>       freeHandle (castPtr err) oci_HTYPE_ERROR
>       freeHandle (castPtr env) oci_HTYPE_ENV
>     return nullPtr
>   )


> getSessionHandle :: EnvHandle -> ErrorHandle -> IO SessHandle
> getSessionHandle env err = catchOCI ( do
>     session <- OCI.handleAlloc oci_HTYPE_SESSION (castPtr env)
>     return (castPtr session)
>   ) (\ociexc -> do
>     convertAndRethrow err ociexc $ do
>       freeHandle (castPtr err) oci_HTYPE_ERROR
>       freeHandle (castPtr env) oci_HTYPE_ENV
>     return nullPtr
>   )


|The idea with multiple logons is to first connect to the server.
Then you create a connection and a session, set the user id details,
and begin the session.
When finished, you end the session,
detach from the server, and free the handles.
So we have one EnvHandle and one ErrorHandle (globally),
and then one ServerHandler, one ConnHandle, and one SessHandle per session.
At the moment we're being lazy,
and not reusing the Env and ErrorHandles for new connections.


> logon :: String -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
> logon user pswd dbname env err = catchOCI ( do
>     server <- getServer env err
>     OCI.serverAttach err server dbname
>     conn <- getConnection env err
>     -- the connection holds a reference to the server in one of its attributes
>     OCI.setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr server) oci_ATTR_SERVER
>     session <- getSessionHandle env err
>     if (user == "")
>       then do
>         OCI.sessionBegin err conn session oci_CRED_EXT
>       else do
>         OCI.setHandleAttrString err (castPtr session) oci_HTYPE_SESSION user oci_ATTR_USERNAME
>         OCI.setHandleAttrString err (castPtr session) oci_HTYPE_SESSION pswd oci_ATTR_PASSWORD
>         OCI.sessionBegin err conn session oci_CRED_RDBMS
>     -- the connection also holds a reference to the session in one of its attributes
>     OCI.setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr session) oci_ATTR_SESSION
>     return conn
>   ) (\ociexc -> do
>     convertAndRethrow err ociexc $ do
>       freeHandle (castPtr err) oci_HTYPE_ERROR
>       freeHandle (castPtr env) oci_HTYPE_ENV
>     return nullPtr
>   )


> logoff :: ErrorHandle -> ConnHandle -> IO ()  
> logoff err conn = catchOCI (do
>     session <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION
>     server <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SERVER
>     OCI.sessionEnd err conn session
>     OCI.serverDetach err server
>     freeHandle (castPtr session) oci_HTYPE_SESSION
>     freeHandle (castPtr conn) oci_HTYPE_SVCCTX
>     freeHandle (castPtr server) oci_HTYPE_SERVER
>   ) (\ociexc -> convertAndRethrow err ociexc nullAction)




> connect :: String -> String -> String -> IO Session
> connect user pswd dbname = do
>   env <- getEnv
>   err <- getErr env
>   conn <- logon user pswd dbname env err
>   return (Session env err conn)



> disconnect :: Session -> IO ()
> disconnect session = do
>   let
>     env = envHandle session
>     err = errorHandle session
>     conn = connHandle session
>   logoff err conn
>   freeHandle (castPtr err) oci_HTYPE_ERROR
>   freeHandle (castPtr env) oci_HTYPE_ENV
>   OCI.terminate


|Oracle only supports ReadCommitted and Serialisable.
If you ask for RepeatableRead, we must go one better and choose Serialisable
(ReadCommitted is no good because you can get non-reapeatable reads).
Oracle has a ReadOnly mode which will give you RepeatableRead,
but you can't do any updates.
 
Oracle's default (and weakest) behaviour is ReadCommitted;
there's no equivalent for ReadUncommitted.

> beginTrans :: Session -> IsolationLevel -> IO ()
> beginTrans session isolation = do
>   let
>     err = errorHandle session
>     conn = connHandle session
>   catchOCI ( do
>       case isolation of
>         ReadUncommitted -> OCI.beginTrans err conn oci_TRANS_READWRITE
>         ReadCommitted -> OCI.beginTrans err conn oci_TRANS_READWRITE
>         RepeatableRead -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>         Serialisable -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>         Serializable -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>     ) (\ociexc -> convertAndRethrow err ociexc nullAction)



> commitTrans :: Session -> IO ()
> commitTrans session = do
>   let
>     err = errorHandle session
>     conn = connHandle session
>   catchOCI ( do
>       OCI.commitTrans err conn
>     ) (\ociexc -> convertAndRethrow err ociexc nullAction)


> rollbackTrans :: Session -> IO ()
> rollbackTrans session = do
>   let
>     err = errorHandle session
>     conn = connHandle session
>   catchOCI ( do
>       OCI.rollbackTrans err conn
>     ) (\ociexc -> convertAndRethrow err ociexc nullAction)


> getStmt :: Session -> IO StmtHandle
> getStmt session = do
>   let
>     env = envHandle session
>     err = errorHandle session
>   catchOCI ( do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       return (castPtr stmt)
>     ) (\ociexc -> do
>       convertAndRethrow err ociexc nullAction
>       return undefined
>     )


> closeStmt :: Session -> StmtHandle -> IO ()
> closeStmt _ stmt = freeHandle (castPtr stmt) oci_HTYPE_STMT


> setPrefetchCount :: Session -> StmtHandle -> Int -> IO ()
> setPrefetchCount session stmt count = do
>   let err = errorHandle session
>   catchOCI ( do
>       with count $ \countPtr -> do
>         OCI.setHandleAttr err (castPtr stmt) oci_HTYPE_STMT countPtr oci_ATTR_PREFETCH_ROWS
>     ) (\ociexc -> do
>       convertAndRethrow err ociexc (closeStmt session stmt)
>     )


> prepare :: Session -> StmtHandle -> String -> IO ()
> prepare session stmt sql = do
>   let err = errorHandle session
>   catchOCI ( do
>       OCI.stmtPrepare err stmt sql
>       --setPrefetchCount session stmt 1000
>     ) (\ociexc -> do
>       convertAndRethrow err ociexc (closeStmt session stmt)
>     )


> word32ToInt :: Word32 -> Int
> word32ToInt n = fromIntegral n

> getRowCount :: Session -> StmtHandle -> IO Int
> getRowCount session stmt = do
>   let err = errorHandle session
>   catchOCI ( do
>       rc <- OCI.getHandleAttr err (castPtr stmt) oci_HTYPE_STMT oci_ATTR_ROW_COUNT
>       return (word32ToInt rc)
>     ) (\ociexc -> do
>       convertAndRethrow err ociexc (closeStmt session stmt)
>       return undefined
>     )


> execute :: Session -> StmtHandle -> Int -> IO Int
> execute session stmt iterations = do
>   let
>     err = errorHandle session
>     conn = connHandle session
>   catchOCI ( do
>       OCI.stmtExecute err conn stmt iterations
>       rowcount <- getRowCount session stmt
>       return rowcount
>     ) (\ociexc -> do
>       convertAndRethrow err ociexc (closeStmt session stmt)
>       return undefined
>     )



> fetchRow :: Session -> Query -> IO CInt
> fetchRow session query = do
>   let
>      err = errorHandle session
>      stmt = stmtHandle query
>   catchOCI ( do
>       rc <- OCI.stmtFetch err stmt
>       return rc
>     ) (\ociexc -> do
>       -- cleanup handled by doQuery1Maker
>       convertAndRethrow err ociexc nullAction
>       return undefined
>     )



> defineCol :: Session -> Query -> Int -> Int -> CInt -> IO OCI.ColumnInfo
> defineCol session query posn bufsize sqldatatype = do
>   let
>      err = errorHandle session
>      stmt = stmtHandle query
>   catchOCI ( do
>       v <- OCI.defineByPos err stmt posn bufsize sqldatatype
>       return v
>     ) (\ociexc -> do
>       convertAndRethrow err ociexc (closeStmt session stmt)
>       return undefined
>     )



--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------



> instance MonadSession (ReaderT Session IO) IO Session where
>   runSession = runReaderT
>   getSession = ask
>
>   beginTransaction isolation = do
>     sess <- ask
>     liftIO $ beginTrans sess isolation
>
>   commit = do
>     sess <- ask
>     liftIO $ commitTrans sess
>
>   rollback = do
>     sess <- ask
>     liftIO $ rollbackTrans sess
>
>   executeDML cmdText = do
>     sess <- ask
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt cmdText
>     rc <- liftIO $ execute sess stmt 1
>     liftIO $ closeStmt sess stmt
>     return rc
>
>   executeDDL cmdText = do
>     _ <- executeDML cmdText
>     return ()



--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> type OCIMonadQuery = ReaderT Query (ReaderT Session IO)

> instance MonadQuery OCIMonadQuery (ReaderT Session IO) Query where
>
>   -- so, doQuery is essentially the result of applying lfold_nonrec_to_rec
>   -- to doQuery1Maker
>   doQuery = doQueryTuned defaultResourceUsage
>
>   doQueryTuned resourceUsage sqltext iteratee seed = do
>     (lFoldLeft, finalizer) <- doQuery1Maker sqltext iteratee resourceUsage
>     catchReaderT (fix lFoldLeft iteratee seed)
>       (\e -> do
>         finalizer
>         liftIO $ throwIO e
>       )
>
>   openCursor = openCursorTuned defaultResourceUsage
>
>   -- This is like 
>   -- lfold_nonrec_to_stream lfold' =
>   -- from the fold-stream.lhs paper:
>   openCursorTuned resourceUsage sqltext iteratee seed = do
>     ref <- liftIO$ newIORef (seed, Nothing)
>     (lFoldLeft, finalizer) <- doQuery1Maker sqltext iteratee resourceUsage
>     let update v = liftIO $ modifyIORef ref (\ (_, f) -> (v, f))
>     let
>       close finalseed = do
>         liftIO$ modifyIORef ref (\_ -> (finalseed, Nothing))
>         finalizer
>         return $ DBCursor ref
>     let
>       k' fni seed' = 
>         let
>           k fni' seed'' = do
>             let k'' flag = if flag then k' fni' seed'' else close seed''
>             liftIO$ modifyIORef ref (\_->(seed'', Just k''))
>             return seed''
>         in do
>           liftIO$ modifyIORef ref (\_ -> (seed', Nothing))
>           do {lFoldLeft k fni seed' >>= update}
>           return $ DBCursor ref
>     k' iteratee seed
>
>   getQuery = ask
>
>   makeQuery sqltext resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt sqltext
>     liftIO $ setPrefetchCount sess stmt (prefetchRowCount resourceUsage)
>     _ <- liftIO $ execute sess stmt 0
>     return $ Query stmt resourceUsage
>
>   doQuery1Maker sqltext iteratee resourceUsage = do
>     sess <- getSession
>     query <- makeQuery sqltext resourceUsage
>     let inQuery m = runReaderT m query
>     buffers <- inQuery $ allocBuffers iteratee 1
>     let
>       finaliser = liftIO $ closeStmt sess (stmtHandle query)
>       hFoldLeft self iteratee seedVal = 
>         runfetch inQuery finaliser buffers self iteratee seedVal
>     return (hFoldLeft, inQuery finaliser)
>
>   fetch1 = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ fetchRow sess query
>     if rc == oci_NO_DATA then return False else return True




--------------------------------------------------------------------
-- ** Result-set data buffers implementation
--------------------------------------------------------------------

If you need to add new database types, then you must:
  In Database.Enumerator:
    - add the constructor to DBColumnType
    - add a fetch function to class Buffer
  In Database.Oracle.OCIEnumerator:
    - add a case for the new constructor to Database.Oracle.OCIEnumerator.dbColumnTypeToCInt
    - add the fetch function to OCIMonadQuery
    - add an instances to DBType: one for the Maybe and one for the raw type.

e.g. adding a fictional type Money:

In Database.Enumerator:

  data DBColumnType =
      DBTypeInt
    ...
    | DBTypeMoney

  class (Monad m) => Buffer m bufferType | m -> bufferType where
    ...
    fetchMoneyVal :: bufferType -> m (Maybe Money)

In Database.Oracle.OCIEnumerator:

  -- This is the tricky bit: extracting the Money value from the column buffer.
  bufferToMoney :: ColumnBuffer -> IO (Maybe Money)
  bufferToMoney buffer = maybeBufferNull buffer $ do ...

  dbColumnTypeToCInt DBTypeMoney = oci_SQLT_MNY

  instance Buffer OCIMonadQuery ColumnBuffer where
    ...
    fetchMoneyVal buffer = liftIO $ bufferToMoney buffer

  instance DBType Money where
    allocBufferFor _ n = allocBuffer (20, DBTypeMoney) n
    fetchCol buffer = throwIfDBNull buffer fetchMoneyVal

  instance DBType (Maybe Money) where
    allocBufferFor _ n = allocBuffer (20, DBTypeMoney) n
    fetchCol buffer = fetchMoneyVal buffer

--------------------------------------------------------------------


> data ColumnBuffer = ColumnBuffer 
>    { bufferFPtr :: OCI.ColumnResultBuffer
>    , nullIndFPtr :: ForeignPtr CShort
>    , retSizeFPtr :: ForeignPtr CShort
>    , bufSize :: Int
>    , colPos :: Int
>    , bufType :: CInt
>    }

> nullByte :: CChar
> nullByte = 0

> cShort2Int :: CShort -> Int
> cShort2Int n = fromIntegral n

> cuCharToInt :: CUChar -> Int
> cuCharToInt c = fromIntegral c

> byteToInt :: Ptr CUChar -> Int -> IO Int
> byteToInt buffer n = do
>   b <- peekByteOff buffer n
>   return (cuCharToInt b)



|Short-circuit null test: if the buffer contains a null then return Nothing.
Otherwise, run the IO action to extract a value from the buffer and return Just it.

> maybeBufferNull :: ColumnBuffer -> IO a -> IO (Maybe a)
> maybeBufferNull buffer action =
>   withForeignPtr (nullIndFPtr buffer) $ \nullIndPtr -> do
>     nullInd <- liftM cShort2Int (peek nullIndPtr)
>     if (nullInd == -1)  -- -1 == null, 0 == value
>       then return Nothing
>       else do
>         v <- action
>         return (Just v)


> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer =
>   maybeBufferNull buffer $
>     -- Given a column buffer, extract a string of variable length
>     -- (you have to terminate it yourself).
>     withForeignPtr (bufferFPtr buffer) $ \bufferPtr ->
>     withForeignPtr (retSizeFPtr buffer) $ \retSizePtr -> do
>       retsize <- liftM cShort2Int (peek retSizePtr)
>       pokeByteOff (castPtr bufferPtr) retsize nullByte
>       val <- peekCString (castPtr bufferPtr)
>       return val


| Oracle's excess-something-or-other encoding for years.

> makeYear :: Int -> Int -> Int
> makeYear c100 y100 =
>   if c100 > 99
>   then 100 * (c100 - 100) + (y100 - 100)
>   else -(100 * (100 - c100) + (100 - y100))


> bufferToDatetime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToDatetime colbuf = maybeBufferNull colbuf $ withForeignPtr (bufferFPtr colbuf) $ \bufferPtr -> do
>   let buffer = castPtr bufferPtr
>   century100 <- byteToInt buffer 0
>   year100 <- byteToInt buffer 1
>   month <- byteToInt buffer 2
>   day <- byteToInt buffer 3
>   hour <- byteToInt buffer 4
>   minute <- byteToInt buffer 5
>   second <- byteToInt buffer 6
>   return $ CalendarTime
>     { ctYear = makeYear century100 year100
>     , ctMonth = toEnum (month - 1)
>     , ctDay = day
>     , ctHour = hour - 1
>     , ctMin = minute - 1
>     , ctSec = second - 1
>     , ctPicosec = 0
>     --, ctWDay = Sunday
>     --, ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }


> bufferPeekValue :: (Storable a) => ColumnBuffer -> IO a
> bufferPeekValue buffer = do
>   v <- withForeignPtr (bufferFPtr buffer) $ \bufferPtr -> peek $ castPtr bufferPtr
>   return v

> bufferToA :: (Storable a) => ColumnBuffer -> IO (Maybe a)
> bufferToA buffer = maybeBufferNull buffer (bufferPeekValue buffer)

> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt = bufferToA

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble = bufferToA


> dbColumnTypeToCInt :: DBColumnType -> CInt
> dbColumnTypeToCInt DBTypeInt = oci_SQLT_INT
> dbColumnTypeToCInt DBTypeString = oci_SQLT_CHR
> dbColumnTypeToCInt DBTypeDatetime = oci_SQLT_DAT
> dbColumnTypeToCInt DBTypeDouble = oci_SQLT_FLT



> instance Buffer OCIMonadQuery ColumnBuffer where
>
>   allocBuffer (bufsize, buftype) colpos = do
>     let ociBufferType = dbColumnTypeToCInt buftype
>     query <- getQuery
>     sess <- lift getSession
>     (_, buf, nullptr, sizeptr) <- liftIO $ defineCol sess query colpos bufsize ociBufferType
>     return $ ColumnBuffer
>       { bufferFPtr = buf
>       , nullIndFPtr = nullptr
>       , retSizeFPtr = sizeptr
>       , bufSize = bufsize
>       , colPos = colpos
>       , bufType = ociBufferType
>       }
>
>   columnPosition buffer = return (colPos buffer)
>
>   currentRowNum = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ getRowCount sess (stmtHandle query)
>     return rc
>
>   fetchStringVal buffer = liftIO $ bufferToString buffer
>   fetchIntVal buffer = liftIO $ bufferToInt buffer
>   fetchDoubleVal buffer = liftIO $ bufferToDouble buffer
>   fetchDatetimeVal buffer = liftIO $ bufferToDatetime buffer
>   freeBuffer _ = return ()


> freeBuffers :: (MonadIO m, Buffer m a) => [a] -> m ()
> freeBuffers buffers = mapM_ freeBuffer buffers




> instance DBType String where
>   allocBufferFor _ n = allocBuffer (4000, DBTypeString) n
>   fetchCol buffer = throwIfDBNull buffer fetchStringVal

> instance DBType Int where
>   allocBufferFor _ n = allocBuffer (4, DBTypeInt) n
>   fetchCol buffer = throwIfDBNull buffer fetchIntVal

> instance DBType Double where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = throwIfDBNull buffer fetchDoubleVal

> instance DBType CalendarTime where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDatetime) n
>   fetchCol buffer = throwIfDBNull buffer fetchDatetimeVal



> instance DBType (Maybe String) where
>   allocBufferFor _ n = allocBuffer (4000, DBTypeString) n
>   fetchCol buffer = fetchStringVal buffer

> instance DBType (Maybe Int) where
>   allocBufferFor _ n = allocBuffer (4, DBTypeInt) n
>   fetchCol buffer = fetchIntVal buffer

> instance DBType (Maybe Double) where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = fetchDoubleVal buffer

> instance DBType (Maybe CalendarTime) where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDatetime) n
>   fetchCol buffer = fetchDatetimeVal buffer
