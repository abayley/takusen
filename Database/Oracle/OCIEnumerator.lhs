
|
Module      :  Database.Enumerator.OCIEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Oracle OCI implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Oracle.OCIEnumerator
>   ( Session, connect, disconnect )
> where


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

> between i (l, u) = i >= l && i <= u

> errorSqlState :: Int -> (String, String)
> errorSqlState 0 = ("00", "000")
> -- 02 - no data
> errorSqlState 1403 = ("02", "000")
> errorSqlState 1095 = ("02", "000")
> -- 23 - integrity violation
> errorSqlState 1 = ("23", "000")
> errorSqlState e | e >= 2290 && e <= 2299 = ("23", "000")
> -- 42 - syntax error or access rule violation
> errorSqlState 22 = ("42", "000")
> errorSqlState 251 = ("42", "000")
> errorSqlState e | e `between` (900, 999) = ("42", "000")
> errorSqlState 1031 = ("42", "000")
> errorSqlState e | e `between` (1490, 1493) = ("42", "000")
> errorSqlState e | e `between` (1700, 1799) = ("42", "000")
> errorSqlState e | e `between` (1900, 2099) = ("42", "000")
> errorSqlState e | e `between` (2140, 2289) = ("42", "000")
> errorSqlState e | e `between` (2420, 2424) = ("42", "000")
> errorSqlState e | e `between` (2450, 2499) = ("42", "000")
> errorSqlState e | e `between` (3276, 3299) = ("42", "000")
> errorSqlState e | e `between` (4040, 4059) = ("42", "000")
> errorSqlState e | e `between` (4070, 4099) = ("42", "000")
> -- 08 - connection errors
> errorSqlState 12154 = ("08", "001") -- TNS: can't resolve service name
> -- unspecified error
> errorSqlState _ = ("01", "000")

> throwSqlError e m = do
>   let
>     s@(ssc,sssc) = errorSqlState e
>     ec = case ssc of
>       "XX" -> DBFatal
>       "58" -> DBFatal
>       "57" -> DBFatal
>       "54" -> DBFatal
>       "53" -> DBFatal
>       "08" -> DBFatal
>       _ -> DBError
>   throwDB (ec s e m)

|rethrow converts an OCIException to a DBException.
The third parameter is an IO action that you can use to clean up any handles.
First we get the error message from the Env or ErrorHandle,
and then we run the cleanup action to free any allocated handles.
(Obviously, we must extract the error message _before_ we free the handles.)
If there's no cleanup action required then simply pass nullAction.

> class OCIExceptionHandler a where
>   rethrow :: a -> OCIException -> IO () -> IO b

> instance OCIExceptionHandler ErrorHandle where
>   rethrow err ex finaliser = do
>     (e, m) <- OCI.formatErrorMsg ex err
>     finaliser
>     throwSqlError e m

> instance OCIExceptionHandler EnvHandle where
>   rethrow env ex finaliser = do
>     (e, m) <- OCI.formatEnvMsg ex env
>     finaliser
>     throwSqlError e m


|What do we do if creating the first handle (Environment) fails?
There's no Env- or ErrorHandle to get the error message from,
so do the best we can by constructing a message with formatErrorCodeDesc.
Still throws DBException.

> reportOCIExc :: OCIException -> IO a
> reportOCIExc (OCIException e m) = do
>   let s = OCI.formatErrorCodeDesc e m
>   printError s
>   throwDB (DBError (errorSqlState 0) 0 s)
>   return undefined



--------------------------------------------------------------------
-- ** OCI Function Wrappers
--------------------------------------------------------------------


> data Session = Session 
>   { envHandle :: EnvHandle
>   , errorHandle :: ErrorHandle
>   , connHandle :: ConnHandle
>   }

> data PreparedStatement = PreparedStatement
>   { stmtHandle :: StmtHandle
>   , stmtSession :: Session
>   , stmtResourceUsage :: QueryResourceUsage
>   }

> data Query = Query
>   { queryStmt :: PreparedStatement
>   }


> class FreeHandle ht where dispose :: ht -> IO ()

> instance FreeHandle EnvHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_ENV
> instance FreeHandle ErrorHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_ERROR
> instance FreeHandle ServerHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_SERVER
> instance FreeHandle ConnHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_SVCCTX
> instance FreeHandle SessHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_SESSION
> instance FreeHandle StmtHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_STMT


|Reports and ignores any errors when freeing handles.
Will catch attempts to free invalid (already freed?) handles.

> freeHandle :: OCIHandle -> CInt -> IO ()
> freeHandle ocihandle handleType = catchOCI ( do
>     OCI.handleFree handleType ocihandle
>   ) (\(OCIException e m) -> do
>     let s = OCI.formatErrorCodeDesc e m
>     printError s
>   )

|Assumes that if an exception is raised,
the Env and Error handles should be freed.

> inOCI :: EnvHandle -> ErrorHandle -> IO a -> IO a
> inOCI env err action = catchOCI action $ \e -> do
>   rethrow err e $ do
>     dispose err
>     dispose env


|Does not free handles when exception raised.

> inSession :: Session -> (EnvHandle -> ErrorHandle -> ConnHandle -> IO a) -> IO () -> IO a
> inSession session action finaliser = do
>   let
>     env = envHandle session
>     err = errorHandle session
>     conn = connHandle session
>   catchOCI (action env err conn) (\e -> rethrow err e finaliser)


> getEnv :: IO EnvHandle
> getEnv = catchOCI OCI.envCreate reportOCIExc

> getErr :: EnvHandle -> IO ErrorHandle
> getErr env = catchOCI ( do
>     err <- OCI.handleAlloc oci_HTYPE_ERROR (castPtr env)
>     return (castPtr err)
>   ) (\e -> rethrow env e (dispose env))


> getServer :: EnvHandle -> ErrorHandle -> IO ServerHandle
> getServer env err = inOCI env err $ do
>     server <- OCI.handleAlloc oci_HTYPE_SERVER (castPtr env)
>     return (castPtr server)


> getConnection :: EnvHandle -> ErrorHandle -> IO ConnHandle
> getConnection env err = inOCI env err $ do
>     conn <- OCI.handleAlloc oci_HTYPE_SVCCTX (castPtr env)
>     return (castPtr conn)


> getSessionHandle :: EnvHandle -> ErrorHandle -> IO SessHandle
> getSessionHandle env err = inOCI env err $ do
>     session <- OCI.handleAlloc oci_HTYPE_SESSION (castPtr env)
>     return (castPtr session)


|The idea with multiple logons is to first connect to the server.
Then you create a connection and a session, set the user id details,
and begin the session.
When finished, you end the session,
detach from the server, and free the handles.
So we should have, globally, one EnvHandle and one ErrorHandle,
and then, per session, one ServerHandle, one ConnHandle, and one SessHandle.
Also, for each server (or Instance, in Oracle-speak), we could share
the ServerHandle among the many ConnHandles and SessHandles.
At the moment we're being lazy,
and not reusing the Env and ErrorHandles for new connections.

> startServerSession :: String -> String -> EnvHandle -> ErrorHandle -> ServerHandle -> IO ConnHandle
> startServerSession user pswd env err server = do
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
>     -- and we need to create a valid transaction handle for the connection, too.
>     trans <- OCI.handleAlloc oci_HTYPE_TRANS (castPtr env)
>     OCI.setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr trans) oci_ATTR_TRANS
>     return conn


> logon :: String -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
> logon user pswd dbname env err = inOCI env err $ do
>     server <- getServer env err
>     OCI.serverAttach err server dbname
>     startServerSession user pswd env err server


> logoff :: ErrorHandle -> ConnHandle -> IO ()  
> logoff err conn = catchOCI (do
>     session <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION
>     server <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SERVER
>     OCI.sessionEnd err conn session
>     OCI.serverDetach err server
>     dispose session
>     dispose conn
>     dispose server
>   ) (\e -> rethrow err e nullAction)




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
>   dispose err
>   dispose env
>   OCI.terminate


|Oracle only supports ReadCommitted and Serialisable.
If you ask for RepeatableRead, we must go one better and choose Serialisable
(ReadCommitted is no good because you can get non-reapeatable reads).
Oracle has a ReadOnly mode which will give you RepeatableRead,
but you can't do any updates.
 
Oracle's default (and weakest) behaviour is ReadCommitted;
there's no equivalent for ReadUncommitted.

> beginTrans :: Session -> IsolationLevel -> IO ()
> beginTrans session isolation = inSession session 
>   (\_ err conn -> do
>       case isolation of
>         ReadUncommitted -> OCI.beginTrans err conn oci_TRANS_READWRITE
>         ReadCommitted -> OCI.beginTrans err conn oci_TRANS_READWRITE
>         RepeatableRead -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>         Serialisable -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>         Serializable -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>   ) nullAction



> commitTrans :: Session -> IO ()
> commitTrans session = inSession session
>   (\_ err conn -> OCI.commitTrans err conn)
>   nullAction

> rollbackTrans :: Session -> IO ()
> rollbackTrans session = inSession session
>   (\_ err conn -> OCI.rollbackTrans err conn)
>   nullAction



> getStmt :: Session -> IO StmtHandle
> getStmt session = inSession session
>   (\ env err _ -> do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       return (castPtr stmt)
>   ) nullAction


> closeStmt :: Session -> StmtHandle -> IO ()
> closeStmt _ stmt = dispose stmt


> setPrefetchCount :: Session -> StmtHandle -> Int -> IO ()
> setPrefetchCount session stmt count = inSession session
>   (\_ err _ -> with count $ \countPtr ->
>         OCI.setHandleAttr err (castPtr stmt) oci_HTYPE_STMT countPtr oci_ATTR_PREFETCH_ROWS
>   ) (closeStmt session stmt)



> prepare :: Session -> StmtHandle -> String -> IO ()
> prepare session stmt sql = inSession session
>   (\_ err _ -> OCI.stmtPrepare err stmt sql
>   ) (closeStmt session stmt)


> word32ToInt :: Word32 -> Int
> word32ToInt n = fromIntegral n

> getRowCount :: Session -> StmtHandle -> IO Int
> getRowCount session stmt = inSession session
>   (\_ err _ -> do
>       rc <- OCI.getHandleAttr err (castPtr stmt) oci_HTYPE_STMT oci_ATTR_ROW_COUNT
>       return (word32ToInt rc)
>   ) (closeStmt session stmt)


> execute :: Session -> StmtHandle -> Int -> IO Int
> execute session stmt iterations = inSession session
>   (\_ err conn -> do
>       OCI.stmtExecute err conn stmt iterations
>       getRowCount session stmt
>   ) (closeStmt session stmt)



> fetchRow :: Session -> PreparedStatement -> IO CInt
> fetchRow session stmt = inSession session
>   (\_ err _ -> OCI.stmtFetch err (stmtHandle stmt))
>   nullAction  -- cleanup handled by doQuery1Maker



> defineCol :: Session -> PreparedStatement -> Int -> Int -> CInt -> IO OCI.ColumnInfo
> defineCol session stmt posn bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.defineByPos err (stmtHandle stmt) posn bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))

> bindByPos :: Session -> PreparedStatement -> Int -> CShort -> OCI.BufferPtr -> Int -> CInt -> IO ()
> bindByPos session stmt posn nullind val bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.bindByPos err (stmtHandle stmt) posn nullind val bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))

--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------


> type SessionM = (ReaderT Session IO)

> instance MonadSession SessionM IO Session PreparedStatement where
>   runSession = flip runReaderT
>   getSession = ask
>
>   beginTransaction isolation = do
>     commit
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
>   withStatement sqlText resourceUsage action = do
>     sess <- getSession
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt (OCI.substituteBindPlaceHolders sqlText)
>     v <- action (PreparedStatement stmt sess resourceUsage)
>     liftIO $ closeStmt sess stmt
>     return v
>
>   bindParameters stmt bindActions = do
>     let nats :: [Int]; nats = [1..]
>     sequence_ $ map (\(p, ba) -> ba stmt p) (zip nats bindActions)
>
>   executeStatement stmt =
>     liftIO $ execute (stmtSession stmt) (stmtHandle stmt) 1
>     
>   prepareStatement sqlText resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ getStmt sess
>     liftIO $ prepare sess stmt (OCI.substituteBindPlaceHolders sqlText)
>     return (PreparedStatement stmt sess resourceUsage)
>     
>   freeStatement stmt =
>     liftIO $ closeStmt (stmtSession stmt) (stmtHandle stmt)
>     
>   executeDML sqlText args = do
>     sess <- getSession
>     withStatement sqlText defaultResourceUsage $ \stmt -> do
>       bindParameters stmt args
>       executeStatement stmt
>
>   executeDDL sqlText args = do
>     _ <- executeDML sqlText args
>     return ()



--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> type QueryM = ReaderT Query SessionM

> instance MonadQuery QueryM SessionM PreparedStatement ColumnBuffer Query where
>
>   runQuery = flip runReaderT
>   getQuery = ask
>
>   makeQuery stmt bindacts = do
>     sess <- getSession
>     bindParameters stmt bindacts
>     liftIO $ setPrefetchCount sess (stmtHandle stmt) (prefetchRowCount (stmtResourceUsage stmt))
>     liftIO $ execute sess (stmtHandle stmt) 0
>     return (Query stmt)
>
>   fetchOneRow = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ fetchRow sess (queryStmt query)
>     return (not (rc == oci_NO_DATA))
>
>   allocBuffer (bufsize, buftype) colpos = do
>     let ociBufferType = dbColumnTypeToCInt buftype
>     query <- getQuery
>     sess <- lift getSession
>     (_, buf, nullptr, sizeptr) <- liftIO $ defineCol sess (queryStmt query) colpos bufsize ociBufferType
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
>     rc <- liftIO $ getRowCount sess (stmtHandle (queryStmt query))
>     return rc
>
>   freeBuffer buffer = return ()




--------------------------------------------------------------------
-- ** Result-set data buffers implementation
--------------------------------------------------------------------


> data ColumnBuffer = ColumnBuffer 
>    { bufferFPtr :: OCI.ColumnResultBuffer
>    , nullIndFPtr :: ForeignPtr CShort
>    , retSizeFPtr :: ForeignPtr CShort
>    , bufSize :: Int
>    , colPos :: Int
>    , bufType :: CInt
>    }


> dbColumnTypeToCInt :: DBColumnType -> CInt
> dbColumnTypeToCInt DBTypeInt = oci_SQLT_INT
> dbColumnTypeToCInt DBTypeString = oci_SQLT_CHR
> dbColumnTypeToCInt DBTypeDatetime = oci_SQLT_DAT
> dbColumnTypeToCInt DBTypeDouble = oci_SQLT_FLT


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

> maybeBufferNull :: ColumnBuffer -> Maybe a -> IO a -> IO (Maybe a)
> maybeBufferNull buffer nullVal action =
>   withForeignPtr (nullIndFPtr buffer) $ \nullIndPtr -> do
>     nullInd <- liftM cShort2Int (peek nullIndPtr)
>     if (nullInd == -1)  -- -1 == null, 0 == value
>       then return nullVal
>       else do
>         v <- action
>         return (Just v)


> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer =
>   -- If it's null then return ""
>   maybeBufferNull buffer Nothing $
>     -- Given a column buffer, extract a string of variable length
>     -- (you have to terminate it yourself).
>     withForeignPtr (bufferFPtr buffer) $ \bufferPtr ->
>     withForeignPtr (retSizeFPtr buffer) $ \retSizePtr -> do
>       retsize <- liftM cShort2Int (peek retSizePtr)
>       pokeByteOff (castPtr bufferPtr) retsize nullByte
>       val <- peekCString (castPtr bufferPtr)
>       return val


| Oracle's excess-something-or-other encoding for years:
year = 100*(c - 100) + (y - 100),
c = (year div 100) + 100,
y = (year mod 100) + 100.

+1999 -> 119, 199
+0100 -> 101, 100
+0001 -> 100, 101
-0001 -> 100,  99
-0100 ->  99, 100
-1999 ->  81,   1

> makeYear :: Int -> Int -> Int
> makeYear c100 y100 = 100 * (c100 - 100) + (y100 - 100)

> makeYearByte :: Int -> Word8
> makeYearByte y = fromIntegral ((rem y 100) + 100)

> makeCentByte :: Int -> Word8
> makeCentByte y = fromIntegral ((quot y 100) + 100)


> dumpBuffer :: Ptr Word8 -> IO ()
> dumpBuffer buf = do
>   dumpByte 0
>   dumpByte 1
>   dumpByte 2
>   dumpByte 3
>   dumpByte 4
>   dumpByte 5
>   dumpByte 6
>   putStrLn ""
>   where
>   dumpByte n = do
>     b <- (peekByteOff buf n :: IO Word8)
>     putStr $ (show b) ++ " "


> bufferToDatetime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToDatetime colbuf = maybeBufferNull colbuf Nothing $
>   withForeignPtr (bufferFPtr colbuf) $ \bufferPtr -> do
>     let buffer = castPtr bufferPtr
>     --dumpBuffer (castPtr buffer)
>     century100 <- byteToInt buffer 0
>     year100 <- byteToInt buffer 1
>     month <- byteToInt buffer 2
>     day <- byteToInt buffer 3
>     hour <- byteToInt buffer 4
>     minute <- byteToInt buffer 5
>     second <- byteToInt buffer 6
>     return $ CalendarTime
>       { ctYear = makeYear century100 year100
>       , ctMonth = toEnum (month - 1)
>       , ctDay = day
>       , ctHour = hour - 1
>       , ctMin = minute - 1
>       , ctSec = second - 1
>       , ctPicosec = 0
>       , ctWDay = Sunday
>       , ctYDay = -1
>       , ctTZName = "UTC"
>       , ctTZ = 0
>       , ctIsDST = False
>       }

> setBufferByte :: OCI.BufferPtr -> Int -> Word8 -> IO ()
> setBufferByte buf n v =
>   pokeByteOff buf n v

> dateTimeToBuffer :: CalendarTime -> OCI.BufferPtr -> IO ()
> dateTimeToBuffer ct buf = do
>   setBufferByte buf 0 (makeCentByte (ctYear ct))
>   setBufferByte buf 1 (makeYearByte (ctYear ct))
>   setBufferByte buf 2 (fromIntegral ((fromEnum (ctMonth ct)) + 1))
>   setBufferByte buf 3 (fromIntegral (ctDay ct))
>   setBufferByte buf 4 (fromIntegral (ctHour ct + 1))
>   setBufferByte buf 5 (fromIntegral (ctMin ct + 1))
>   setBufferByte buf 6 (fromIntegral (ctSec ct + 1))



> bufferPeekValue :: (Storable a) => ColumnBuffer -> IO a
> bufferPeekValue buffer = do
>   v <- withForeignPtr (bufferFPtr buffer) $ \bufferPtr -> peek $ castPtr bufferPtr
>   return v

> bufferToA :: (Storable a) => ColumnBuffer -> IO (Maybe a)
> bufferToA buffer = maybeBufferNull buffer Nothing (bufferPeekValue buffer)

> bufferToCInt :: ColumnBuffer -> IO (Maybe CInt)
> bufferToCInt = bufferToA

> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt b = do
>   cint <- bufferToCInt b
>   return $ maybe Nothing (Just . fromIntegral) cint

> bufferToCDouble :: ColumnBuffer -> IO (Maybe CDouble)
> bufferToCDouble = bufferToA

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble b = do
>   cdbl <- bufferToCDouble b
>   return $ maybe Nothing (Just . realToFrac) cdbl

|Turn a @Maybe a@ value into a @(null_ind, value)@ pair.

> maybeToInd :: Maybe b -> (b -> a) -> a -> (CShort, a)
> maybeToInd mv conv nullVal = maybe (-1, nullVal) (\v -> (0, conv v)) mv


> instance DBBind (Maybe String) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     let sess = stmtSession stmt
>     let (nullind, s) = maybeToInd val id ""
>     liftIO$ withCStringLen s $ \(cstr, clen) -> 
>       bindByPos sess stmt pos nullind (castPtr cstr) clen oci_SQLT_CHR

> instance DBBind (Maybe Int) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     let sess = stmtSession stmt
>     let (nullind, i) = maybeToInd val (fromIntegral::Int -> CInt) 0
>     liftIO$ alloca $ \valPtr -> do
>       poke valPtr i
>       bindByPos sess stmt pos nullind (castPtr valPtr) (sizeOf i) oci_SQLT_INT

> instance DBBind (Maybe Double) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     let sess = stmtSession stmt
>     let (nullind, i) = maybeToInd val (fromRational . toRational :: Double -> CDouble) 0
>     liftIO$ alloca $ \valPtr -> do
>       poke valPtr i
>       bindByPos sess stmt pos nullind (castPtr valPtr) (sizeOf i) oci_SQLT_FLT

> instance DBBind (Maybe CalendarTime) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     let sess = stmtSession stmt
>     liftIO$ allocaBytes 8 $ \bufptr -> do
>       (nullind, d) <- case val of
>         Nothing -> return (-1, bufptr)
>         Just ct -> do
>           dateTimeToBuffer ct bufptr
>           return (0, bufptr)
>       bindByPos sess stmt pos nullind (castPtr bufptr) 7 oci_SQLT_DAT

> instance DBBind (Maybe a) SessionM PreparedStatement
>     => DBBind a SessionM PreparedStatement where
>   bindPos stmt val pos = bindPos stmt (Just val) pos

> instance (Show a, Read a) => DBBind (Maybe a) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     case val of
>       Just v -> bindPos stmt (Just (show v)) pos
>       Nothing -> bindPos stmt (Nothing :: Maybe String) pos



> instance DBType (Maybe String) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (16000, DBTypeString) n
>   fetchCol buffer = liftIO$ bufferToString buffer

> instance DBType (Maybe Int) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (4, DBTypeInt) n
>   fetchCol buffer = liftIO$ bufferToInt buffer

> instance DBType (Maybe Double) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = liftIO$ bufferToDouble buffer

> instance DBType (Maybe CalendarTime) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (7, DBTypeDatetime) n
>   fetchCol buffer = liftIO$ bufferToDatetime buffer

|This single polymorphic instance covers all of the
type-specific non-Maybe instances e.g. String, Int, Double, etc.

> instance DBType (Maybe a) QueryM ColumnBuffer
>     => DBType a QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBufferFor (undefined::Maybe a) n
>   fetchCol buffer = throwIfDBNull buffer fetchCol


|A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (16000, DBTypeString) n
>   fetchCol buffer = do
>     v <- liftIO$ bufferToString buffer
>     return $ maybe Nothing (Just . read) v
