{-|
Module      :  Database.Enumerator.OCIEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainers :  oleg@pobox.com, alistair@abayley.org
Stability   :  unstable
Portability :  non-portable

Oracle OCI implementation of Database.Enumerator.
-}

> {-# OPTIONS -fglasgow-exts #-}

> module Database.Oracle.OCIEnumerator where


> import Database.Enumerator
> import Database.Oracle.OCIConstants
> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions (OCIHandle, EnvHandle, ErrorHandle, ConnHandle, StmtHandle, OCIException (..), catchOCI)
> import Foreign
> import Foreign.C
> import Foreign.C.Types
> import Foreign.Marshal
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
-- Error handling
--------------------------------------------------------------------



> nullAction :: IO ()
> nullAction = return ()

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

convertAndRethrow converts an OCIException to a DBException.
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


What do we do if the first handle (Environment) fails?
There's no Env- or ErrorHandle to get the error message from,
so do the best we can by constructing a message with formatErrorCodeDesc.
Still throws DBException.

> reportOCIExc :: OCIException -> IO ()
> reportOCIExc (OCIException e m) = do
>   let s = OCI.formatErrorCodeDesc e m
>   printError s
>   throwDB (DBError 0 s)


This is a version of convertAndRethrow version that uses EnvHandle
rather than ErrorHandle. Only used by getErr.

> convertAndRethrowEnv :: EnvHandle -> OCIException -> IO () -> IO ()
> convertAndRethrowEnv env ociexc cleanupAction = do
>   (e, m) <- OCI.formatEnvMsg ociexc env
>   cleanupAction
>   throwDB (DBError e m)


--------------------------------------------------------------------
-- Wrappers for OCI functions, which catch and convert exceptions.
--------------------------------------------------------------------


> data Session = Session 
>   { envHandle :: EnvHandle
>   , errorHandle :: ErrorHandle
>   , connHandle :: ConnHandle
>   }

> data Query = Query { stmtHandle :: StmtHandle }


Reports and ignores any errors when freeing handles.
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


> logon :: String -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
> logon user pswd dbname env err = catchOCI ( do
>     connection <- OCI.dbLogon user pswd dbname env err
>     return connection
>   ) (\ociexc -> do
>     convertAndRethrow err ociexc $ do
>       freeHandle (castPtr err) oci_HTYPE_ERROR
>       freeHandle (castPtr env) oci_HTYPE_ENV
>     return nullPtr
>   )


> logoff :: ErrorHandle -> ConnHandle -> IO ()  
> logoff err conn = catchOCI ( do
>     OCI.dbLogoff err conn
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


Oracle only supports ReadCommitted and Serialisable.
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


> prepare :: Session -> StmtHandle -> String -> IO ()
> prepare session stmt sql = do
>   let err = errorHandle session
>   catchOCI ( do
>       OCI.stmtPrepare err stmt sql
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
-- Sessions
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



To manage multiple sessions... don't use logon. Do this instead:

env <- envCreate
err <- getErr env
server <- getServer env err
serverAttach err server "database"
conn <- getConnection env
setAttrHandle err conn oci_HTYPE_SVCCTX server oci_ATTR_SERVER
session <- getSession err
setAttrString err sess oci_HTYPE_SESSION "username" oci_ATTR_USERNAME
setAttrString err sess oci_HTYPE_SESSION "password" oci_ATTR_PASSWORD
sessionBegin err conn session
setAttrHandle err conn oci_HTYPE_SVCCTX session oci_ATTR_SESSION
... do queries etc ...
sessionEnd err conn session
serverDetach err server
handleFree server
handleFree error
handleFree env



 getServer :: EnvHandle -> ErrorHandle -> IO ServerHandle
 getServer envHandle errHandle = catchDyn ( do
     serverHandle <- OCI.handleAlloc oci_HTYPE_SERVER envHandle
     return serverHandle
   ) (\(ociexc :: OCIException) -> do
     convertAndRethrow errHandle ociexc $ do
       OCI.handleFree oci_HTYPE_ERROR errHandle
       OCI.handleFree oci_HTYPE_ENV envHandle
     return envHandle
   )





--------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------



> type OCIMonadQuery = ReaderT Query (ReaderT Session IO)

> instance MonadQuery OCIMonadQuery (ReaderT Session IO) Query where
>
>   makeQuery sqltext = do
>     sess <- getSession
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt sqltext
>     _ <- liftIO $ execute sess stmt 0
>     return $ Query stmt
>
>   getQuery = ask
>
>   -- so, doQuery is essentially the result of applying lfold_nonrec_to_rec
>   -- to doQuery1Maker
>   doQuery sqltext iteratee seedVal = do
>     (lFoldLeft, finalizer) <- doQuery1Maker sqltext iteratee
>     catchReaderT (fix lFoldLeft iteratee seedVal)
>       (\e -> do
>         finalizer
>         liftIO $ throwIO e
>       )
>
>   doQuery1Maker sqltext iteratee = do
>     sess <- getSession
>     query <- makeQuery sqltext
>     let inQuery m = runReaderT m query
>     buffers <- inQuery $ allocBuffers iteratee 1
>     let
>       finaliser = do
>         freeBuffers buffers
>         liftIO $ closeStmt sess (stmtHandle query)
>       hFoldLeft self iteratee seedVal = 
>         inQuery $ runfetch finaliser buffers self iteratee seedVal
>     return (hFoldLeft, inQuery finaliser)
>
>   -- This is like 
>   -- lfold_nonrec_to_stream lfold' =
>   -- from the fold-stream.lhs paper:
>   openCursor sqltext iteratee seedVal = do
>     ref <- liftIO$ newIORef (seedVal,Nothing)
>     (lFoldLeft, finalizer) <- doQuery1Maker sqltext iteratee
>     let update v = liftIO $ modifyIORef ref (\ (_, f) -> (v, f))
>     let
>       close seed = do
>         liftIO$ modifyIORef ref (\_ -> (seed,Nothing))
>         finalizer
>         return $ DBCursor ref
>     let
>       k' fni seed = 
>         let
>           k fni seed = do
>             let k'' flag = if flag then k' fni seed else close seed
>             liftIO$ modifyIORef ref (\_->(seed, Just k''))
>             return seed
>         in do
>           liftIO$ modifyIORef ref (\_ -> (seed, Nothing))
>           do {lFoldLeft k fni seed >>= update}
>           return $ DBCursor ref
>     k' iteratee seedVal
>
>   fetch1 = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ fetchRow sess query
>     if rc == oci_NO_DATA then return False else return True



--------------------------------------------------------------------
-- result-set data buffers implementation
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
>    { bufPtr :: OCI.ColumnResultBuffer
>    , nullIndPtr :: Ptr CShort
>    , retSizePtr :: Ptr CShort
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



Short-circuit null test: if the buffer contains a null then return Nothing.
Otherwise, run the IO action to extract a value from the buffer and return Just it.

> maybeBufferNull :: ColumnBuffer -> IO a -> IO (Maybe a)
> maybeBufferNull buffer action = do
>   nullInd <- liftM cShort2Int (peek (nullIndPtr buffer))
>   if (nullInd == -1)  -- -1 == null, 0 == value
>     then return Nothing
>     else do
>       v <- action
>       return (Just v)


> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer = maybeBufferNull buffer $ do
>     -- Given a column buffer, extract a string of variable length
>     -- (you have to terminate it yourself).
>     retsize <- liftM cShort2Int (peek (retSizePtr buffer))
>     pokeByteOff (castPtr (bufPtr buffer)) retsize nullByte
>     val <- peekCString (castPtr (bufPtr buffer))
>     return val


Oracle's excess-something-or-other encoding for years:

> makeYear :: Int -> Int -> Int
> makeYear c100 y100 =
>   if c100 > 99
>   then 100 * (c100 - 100) + (y100 - 100)
>   else -(100 * (100 - c100) + (100 - y100))


> bufferToDatetime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToDatetime colbuf = maybeBufferNull colbuf $ do
>   let buffer = castPtr (bufPtr colbuf)
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
>   v <- peek $ castPtr $ bufPtr buffer
>   return v

> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt buffer = maybeBufferNull buffer (bufferPeekValue buffer)

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble buffer = maybeBufferNull buffer (bufferPeekValue buffer)


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
>       { bufPtr = buf
>       , nullIndPtr = nullptr
>       , retSizePtr = sizeptr
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
>   freeBuffer buffer = liftIO $ do  -- Free a single column's buffer.
>     free (bufPtr buffer)
>     free (retSizePtr buffer)
>     free (nullIndPtr buffer)


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
