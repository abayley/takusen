
|
Module      :  Database.Enumerator.OCIStub
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Stub Oracle OCI implementation of Database.Enumerator.
Good for people who can't or won't install Oracle,
so that they can try out the Enumerator interface.
 
Currently last last row of any fetch will have a null in its Int columns
(this makes it easier to test handling of nulls and DBUnexpectedNull).
See fetchIntVal.



> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Oracle.OCIStub where



> import Database.Enumerator
> import Foreign
> import Foreign.C
> import Foreign.C.Types
> import Control.Monad
> import Control.Monad.State
> import Control.Exception (catchDyn, throwDyn, throwIO)
> import System.Time
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.IORef
> import Data.Dynamic


----------------------------------------------------------------
-- Normally imported from Database.Oracle.OCIFunctions
----------------------------------------------------------------

> oci_SQLT_CHR :: CInt
> oci_SQLT_CHR = 1
> oci_SQLT_INT :: CInt
> oci_SQLT_INT = 2
> oci_SQLT_FLT :: CInt
> oci_SQLT_FLT = 2
> oci_SQLT_DAT :: CInt
> oci_SQLT_DAT = 2

> oci_NO_DATA :: CInt
> oci_NO_DATA = 100

> data OCIStruct = OCIStruct
> type OCIHandle = Ptr OCIStruct  -- generic Handle for OCI functions that return Handles
> data OCIBuffer = OCIBuffer  -- generic buffer. Could hold anything: value or pointer.
> type BufferPtr = Ptr OCIBuffer
> type ColumnResultBuffer = BufferPtr


> data EnvStruct = EnvStruct
> type EnvHandle = Ptr EnvStruct
> data ErrorStruct = ErrorStruct
> type ErrorHandle = Ptr ErrorStruct
> data ServerStruct = ServerStruct
> type ServerHandle = Ptr ServerStruct
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
> type ColumnInfo = (DefnHandle, BufferPtr, Ptr CShort, Ptr CShort)



> data OCIException = OCIException CInt String
>   deriving (Typeable, Show)

> catchOCI :: IO a -> (OCIException -> IO a) -> IO a
> catchOCI = catchDyn

> throwOCI :: OCIException -> a
> throwOCI = throwDyn


--------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------



> nullAction :: IO ()
> nullAction = return ()

reportAndRethrow converts an OCIException to a DBException.
The third parameter is an IO action that you can use to clean up any handles.
First we get the error message from the ErrorHandle,
and then we run the cleanup action to free any allocated handles.
(Obviously, we must extract the error message _before_ we free the handles.)
If there's no cleanup action required then simply pass nullAction.

> reportAndRethrow :: ErrorHandle -> OCIException -> IO () -> IO ()
> reportAndRethrow err (OCIException e m) cleanupAction = do
>   putStrLn m
>   cleanupAction
>   throwDB (DBError 0 m)


> reportAndIgnore :: ErrorHandle -> OCIException -> IO () -> IO ()
> reportAndIgnore err (OCIException e m) cleanupAction = do
>   putStrLn m
>   cleanupAction


What do we do if the first handle (Environment) fails?
There's no Env- or ErrorHandle to get the error message from,
so do the best we can by constructing a message with formatErrorCodeDesc.
Still throws DBException.

> reportOCIExc :: OCIException -> IO ()
> reportOCIExc (OCIException e m) = do
>   putStrLn m
>   throwDB (DBError 0 m)


This is a version of reportAndRethrow version that uses EnvHandle
rather than ErrorHandle. Only used by getErr.

> reportAndRethrowEnv :: EnvHandle -> OCIException -> IO () -> IO ()
> reportAndRethrowEnv env (OCIException e m) cleanupAction = do
>   putStrLn m
>   cleanupAction
>   throwDB (DBError 0 m)


--------------------------------------------------------------------
-- Stubs for OCI function wrappers.
--------------------------------------------------------------------


> data Session = Session 
>   { envHandle :: EnvHandle
>   , errorHandle :: ErrorHandle
>   , connHandle :: ConnHandle
>   }

> data Query = Query
>   { stmtHandle :: StmtHandle
>   , fetchCounter :: IORef (IORef Int)
>   }



> connect :: String -> String -> String -> IO Session
> connect user pswd dbname = return (Session nullPtr nullPtr nullPtr)



> disconnect :: Session -> IO ()
> disconnect session = return ()


Oracle only supports ReadCommitted and Serialisable.
If you ask for RepeatableRead, we must go one better and choose Serialisable
(ReadCommitted is no good because you can get non-reapeatable reads).
Oracle's default (and weakest) behaviour is ReadCommitted;
there's no equivalent for ReadUncommitted.

Oracle has a ReadOnly mode which will give you RepeatableRead,
but you can't do any updates.

> beginTrans :: Session -> IsolationLevel -> IO ()
> beginTrans session isolation = return ()

> commitTrans :: Session -> IO ()
> commitTrans session = return ()

> rollbackTrans :: Session -> IO ()
> rollbackTrans session = return ()

> getStmt :: Session -> IO StmtHandle
> getStmt session = return nullPtr

> closeStmt :: Session -> StmtHandle -> IO ()
> closeStmt session stmt = return ()

> prepare :: Session -> StmtHandle -> String -> IO ()
> prepare session stmt sql = return ()

> getRowCount :: Session -> StmtHandle -> IO Int
> getRowCount session stmt = return 0

> execute :: Session -> StmtHandle -> Int -> IO Int
> execute session stmt iterations = return 0

> fetchRow :: Session -> Query -> IO CInt
> fetchRow session query = return 0

> defineCol :: Session -> Query -> Int -> Int -> CInt -> IO ColumnInfo
> defineCol session query posn bufsize sqldatatype =
>     return (nullPtr, nullPtr, nullPtr, nullPtr)


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



--------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------

See makeQuery below for use of this:

> numberOfRowsToPretendToFetch :: Int
> numberOfRowsToPretendToFetch = 100000

See fetchIntVal below for use of this:

> throwNullIntOnRow :: Int
> throwNullIntOnRow = 1



> type OCIMonadQuery = ReaderT Query (ReaderT Session IO)

> instance MonadQuery OCIMonadQuery (ReaderT Session IO) Query where
>
>   makeQuery sqltext = do
>     sess <- getSession
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt sqltext
>     _ <- liftIO $ execute sess stmt 0
>     --return $ Query stmt
>     -- Leave one counter in to ensure the fetch terminates
>     counter <- liftIO $ newIORef numberOfRowsToPretendToFetch >>= newIORef
>     return $ Query stmt counter
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
>             liftIO$ modifyIORef ref (\_->(seed,Just k''))
>             return seed
>         in do
>           liftIO$ modifyIORef ref (\_ -> (seed,Nothing))
>           do {lFoldLeft k fni seed >>= update}
>           return $ DBCursor ref
>     k' iteratee seedVal
>
>   fetch1 = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ fetchRow sess query
>     --if rc == oci_NO_DATA then return False else return True
>     --
>     -- We'll pretend that we're going to fetch a finite number of rows.
>     refCounter <- liftIO $ readIORef (fetchCounter query)
>     counter <- liftIO $ readIORef refCounter
>     if counter >= 0
>       then (liftIO $ writeIORef refCounter (counter - 1) >> return True)
>       else return False




--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------


> data ColumnBuffer = ColumnBuffer 
>    { bufPtr :: ColumnResultBuffer
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
>   if (nullInd == -1)
>     then return Nothing
>     else do
>       v <- action
>       return (Just v)


> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer = return $ Just "boo"

> bufferToDatetime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToDatetime colbuf = do
>       return $ Just $ CalendarTime
>         { ctYear = 1971
>         , ctMonth = toEnum 6
>         , ctDay = 1
>         , ctHour = 12
>         , ctMin = 1
>         , ctSec = 1
>         , ctPicosec = 0
>         --, ctWDay = Sunday
>         --, ctYDay = -1
>         , ctTZName = "UTC"
>         , ctTZ = 0
>         , ctIsDST = False
>         }


> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt buffer = return $ Just 1

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble buffer = return $ Just 1.1


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
>     (_, buf, null, sizeptr) <- liftIO $ defineCol sess query colpos bufsize ociBufferType
>     return $ ColumnBuffer
>       { bufPtr = buf
>       , nullIndPtr = null
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
>   fetchIntVal buffer = do
>     query <- getQuery
>     refCounter <- liftIO $ readIORef (fetchCounter query)
>     counter <- liftIO $ readIORef refCounter
>     -- last row returns null rather than 1
>     if counter == throwNullIntOnRow
>       then return Nothing
>       else liftIO $ bufferToInt buffer
>   fetchDoubleVal buffer = liftIO $ bufferToDouble buffer
>   fetchDatetimeVal buffer = liftIO $ bufferToDatetime buffer
>   freeBuffer buffer = return ()


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
