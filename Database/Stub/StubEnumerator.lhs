
|
Module      :  Database.Stub.StubEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Stub implementation of Database.Enumerator.
Useful for people who can't or won't install a DBMS,
so that they can try out the Enumerator interface.
 
Currently last last row of any fetch will have a null in its Int columns
(this makes it easier to test handling of nulls and DBUnexpectedNull).
See fetchIntVal.



> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Stub.StubEnumerator where


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

> setPrefetchCount :: Session -> StmtHandle -> Int -> IO ()
> setPrefetchCount session stmt count = return ()

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
> numberOfRowsToPretendToFetch = 10

See fetchIntVal below for use of this:

> throwNullIntOnRow :: Int
> throwNullIntOnRow = 1



> type OCIMonadQuery = ReaderT Query (ReaderT Session IO)

> instance MonadQuery OCIMonadQuery (ReaderT Session IO) Query ColumnBuffer
>  where
>
>   runQuery m query = runReaderT m query
>
>   getQuery = ask
>
>   makeQuery sqltext resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt sqltext
>     liftIO $ setPrefetchCount sess stmt (prefetchRowCount resourceUsage)
>     _ <- liftIO $ execute sess stmt 0
>     --return $ Query stmt
>     -- Leave one counter in to ensure the fetch terminates
>     counter <- liftIO $ newIORef numberOfRowsToPretendToFetch >>= newIORef
>     return $ Query stmt counter
>
>   fetch1Row = do
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
>
>   allocBuffer (bufsize, buftype) colpos = do
>     let ociBufferType = dbColumnTypeToCInt buftype
>     query <- getQuery
>     sess <- lift getSession
>     (_, buf, null, sizeptr) <- liftIO $ 
>			 defineCol sess query colpos bufsize ociBufferType
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
>   freeBuffer buffer = return ()
>   destroyQuery query = -- after buffers are freed, close the STMT
>                do sess <- getSession
>		    liftIO $ closeStmt sess (stmtHandle query)



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


> instance DBType (Maybe a) OCIMonadQuery ColumnBuffer
>     => DBType a OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol buffer = throwIfDBNull buffer fetchCol

>{-
> instance DBType Double OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = throwIfDBNull buffer fetchDoubleVal
>
> instance DBType CalendarTime OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDatetime) n
>   fetchCol buffer = throwIfDBNull buffer fetchDatetimeVal
>-}


> instance DBType (Maybe String) OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (4000, DBTypeString) n
>   fetchCol buffer = liftIO $ bufferToString buffer

> instance DBType (Maybe Int) OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (4, DBTypeInt) n
>   fetchCol buffer = do
>     query <- getQuery
>     refCounter <- liftIO $ readIORef (fetchCounter query)
>     counter <- liftIO $ readIORef refCounter
>     -- last row returns null rather than 1
>     if counter == throwNullIntOnRow
>       then return Nothing
>       else liftIO $ bufferToInt buffer

> instance DBType (Maybe Double) OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = liftIO $ bufferToDouble buffer

> instance DBType (Maybe CalendarTime) OCIMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDatetime) n
>   fetchCol buffer = liftIO $ bufferToDatetime buffer
