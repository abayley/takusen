
|
Module      :  Database.Oracle.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Oracle OCI implementation of Database.Enumerator.

Oracle has two ways to create additional result-sets.
 1. with nested cursors in the SQL select-statement (bind variables optional)
    This gives us the RefCursor as a result-set column
    i.e. we get it back in the buffer passed to defineByPos.
 2. as output parameters in a PL/SQL block
    This gives us the RefCursor as an output bind variables
    i.e. we get it back in the buffer passed to bindByPos.

Problems:
 - need to prepare anonymous PL/SQL block with bind placeholders for parameters
 - need to have number of parameters when preparing
 - need actual bind values when binding = number must match prepare number
 - need to specify In, Out, In/Out - always use OCIBindByPos
 - when executed (in withBoundStatement/bindRun) need to get return values
   and save them... where? Do we create a tuple; Do we allow a single row fetch?

So we need to add an instance to OracleBind for (RefCursor StmtHandle).
When this binds we will have to save a list of the output bind buffers
somewhere in the statement object (I suppose these can be reused).
How do we return output values back to the program?


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Oracle.Enumerator
>   ( Session, connect
>   , prepareStmt, sql, sqlbind, prefetch
>   , module Database.Enumerator
>   )
> where


> import qualified Database.Enumerator
> import Database.Enumerator (print_, RefCursor(..), NextResultSet(..))
> import qualified Database.Enumerator as Enum (PreparedStmt(..))
> import Database.InternalEnumerator
> import Database.Oracle.OCIConstants
> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions
>   ( OCIHandle, EnvHandle, ErrorHandle, ServerHandle, ConnHandle, SessHandle, StmtHandle
>   , OCIException (..), catchOCI)
> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.IORef
> import Data.Int
> import Data.Time
> import Data.Fixed
> import System.Time
> import System.IO (hPutStrLn, stderr)


--------------------------------------------------------------------
-- ** Error handling
--------------------------------------------------------------------

> nullAction :: IO ()
> nullAction = return ()


> between i (l, u) = i >= l && i <= u

Where did I find these mappings?...

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

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

--------------------------------------------------------------------
-- ** OCI Function Wrappers
--------------------------------------------------------------------

These wrappers ensure that only DBExceptions are thrown,
and never OCIExceptions.

> data Session = Session 
>   { envHandle :: EnvHandle
>   , errorHandle :: ErrorHandle
>   , connHandle :: ConnHandle
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




> dbConnect :: String -> String -> String -> IO Session
> dbConnect user pswd dbname = do
>   env <- getEnv
>   err <- getErr env
>   conn <- logon user pswd dbname env err
>   return (Session env err conn)



> dbDisconnect :: Session -> IO ()
> dbDisconnect session = do
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



> stmtPrepare :: Session -> StmtHandle -> String -> IO ()
> stmtPrepare session stmt sql = inSession session
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


> fetchRow :: Session -> PreparedStmt -> IO CInt
> fetchRow session stmt = inSession session
>   (\_ err _ -> OCI.stmtFetch err (stmtHandle stmt))
>   nullAction  -- cleanup handled by doQuery1Maker


> defineCol :: Session -> PreparedStmt -> Int -> Int -> CInt -> IO OCI.ColumnInfo
> defineCol session stmt posn bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.defineByPos err (stmtHandle stmt) posn bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))

> bindByPos :: Session -> PreparedStmt -> Int -> CShort -> OCI.BufferPtr -> Int -> CInt -> IO ()
> bindByPos session stmt posn nullind val bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.bindByPos err (stmtHandle stmt) posn nullind val bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))


--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

> connect :: String -> String -> String -> ConnectA Session
> connect user pswd dbname = ConnectA (dbConnect user pswd dbname)

--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------

> newtype QueryString = QueryString String

> sql :: String -> QueryString
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand sess (QueryString str) = do
>     stmt <- getStmt sess
>     stmtPrepare sess stmt str
>     n <- execute sess stmt 1
>     closeStmt sess stmt
>     return (fromIntegral n)

> instance Command String Session where
>   executeCommand sess str = executeCommand sess (sql str)

> instance Command BoundStmt Session where
>   executeCommand s (BoundStmt pstmt) = do
>     n <- getRowCount s (stmtHandle pstmt)
>     return n


> instance ISession Session where
>   disconnect sess = dbDisconnect sess
>   beginTransaction sess isolation = beginTrans sess isolation
>   commit sess = commitTrans sess
>   rollback sess = rollbackTrans sess

About stmtFreeWithQuery:

We need to keep track of the scope of the PreparedStmt
i.e. should it be freed when the Query (result-set) is freed,
or does it have a longer lifetime?
PreparedStmts created by prepareStmt have a lifetime possibly
longer than the result-set; users should use withPreparedStatement
to manage these.

PreparedStmts can also be created internally by various instances
of makeQuery (in class Statement), and these will usually have the
same lifetime/scope as that of the Query (result-set).

> data PreparedStmt =
>     PreparedStmt
>       { stmtHandle :: StmtHandle
>       , stmtSession :: Session
>       , stmtCursors :: IORef [RefCursor StmtHandle]
>       , stmtBuffers :: IORef [ForeignPtr Word8]
>       }
>   | PreparedStmtFreeWithQuery
>       { stmtHandle :: StmtHandle
>       , stmtSession :: Session
>       , stmtCursors :: IORef [RefCursor StmtHandle]
>       , stmtBuffers :: IORef [ForeignPtr Word8]
>       }


> prepareStmt :: QueryString -> PreparationA Session PreparedStmt
> prepareStmt (QueryString sqltext) = prepareStmt' sqltext PreparedStmt

> prepareStmt' sqltext constr =
>   PreparationA (\sess -> do
>     stmt <- getStmt sess
>     stmtPrepare sess stmt (OCI.substituteBindPlaceHolders sqltext)
>     newPreparedStmt constr sess stmt
>     )

> newPreparedStmt constr sess stmt = do
>   c <- newIORef []
>   b <- newIORef []
>   return (constr stmt sess c b)

--------------------------------------------------------------------
-- ** Binding
--------------------------------------------------------------------

> newtype BoundStmt = BoundStmt { boundStmt :: PreparedStmt }

We might need to change this, to keep track of the binding-object
returned by the bind call. What type should the bind object be?
Some kind of buffer pointer, I guess... or a function
that returns the value (from the buffer)?

> type BindObj = Int -> IO ()
> newtype Out a = Out a

> instance IPrepared PreparedStmt Session BoundStmt BindObj where
>   bindRun sess stmt bas action = do
>     sequence_ (zipWith (\i (BindA ba) -> ba sess stmt i) [1..] bas)
>     execute sess (stmtHandle stmt) 0
>     writeIORef (stmtCursors stmt) []
>     -- after execute, save any RefCursor Out values...
>     -- or should we save all Out values?
>     action (BoundStmt stmt)
>   destroyStmt sess (PreparedStmtFreeWithQuery stmth _ _ _) = closeStmt sess stmth
>   destroyStmt sess (PreparedStmt _ _ _ _) = return ()

> instance DBBind (Maybe String) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Out (Maybe Int)) Session PreparedStmt BindObj where
>   bindP (Out v) = BindA (\sess stmt pos -> do
>       buffer <- mallocForeignPtrBytes (bindSize v)
>       withForeignPtr buffer (\ptr -> do
>         case v of
>           Nothing -> return ()
>           Just i -> poke (castPtr ptr) (toCInt i)
>         bindByPos sess stmt pos (bindNullInd v) (castPtr ptr) (bindSize v) (bindType v)
>         )
>       appendOutputBindBuffer stmt buffer
>     )

> appendOutputBindBuffer stmt buffer = do
>   modifyIORef (stmtBuffers stmt) (++ [buffer])

 instance DBBind (Maybe Int64) Session PreparedStmt BindObj where
   bindP = makeBindAction

> instance DBBind (Maybe Double) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe CalendarTime) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe UTCTime) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe a) Session PreparedStmt BindObj
>     => DBBind a Session PreparedStmt BindObj where
>   bindP x = bindP (Just x)

The default instance, uses generic Show

> instance (Show a) => DBBind (Maybe a) Session PreparedStmt BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")



> makeBindAction x = BindA (\ses st -> bindMaybe ses st x)

> bindMaybe :: (OracleBind a)
>   => Session -> PreparedStmt -> Maybe a -> Int -> IO ()
> bindMaybe sess stmt v pos =
>   bindWithValue v $ \ptrv -> do
>     bindByPos sess stmt pos (bindNullInd v) (castPtr ptrv) (bindSize v) (bindType v)


> class OracleBind a where
>   bindWithValue :: a -> (Ptr Word8 -> IO ()) -> IO ()
>   bindSize :: a -> Int
>   bindNullInd :: a -> CShort
>   bindType :: a -> CInt

> instance OracleBind a => OracleBind (Maybe a) where
>   bindWithValue (Just v) a = bindWithValue v a
>   bindWithValue Nothing a = return ()
>   bindSize (Just v) = bindSize v
>   bindSize Nothing = 0
>   bindNullInd (Just v) = 0
>   bindNullInd Nothing = -1
>   bindType (Just v) = bindType v
>   bindType Nothing = bindType (undefined :: a)

> instance OracleBind String where
>   bindWithValue v a = withCString v (\p -> a (castPtr p))
>   bindSize s = fromIntegral (length s)
>   bindNullInd _ = 0
>   bindType _ = oci_SQLT_CHR

> instance OracleBind Int where
>   bindWithValue v a = withBinaryValue toCInt v (\p v -> poke (castPtr p) v) a
>   bindSize _ = (sizeOf (toCInt 0))
>   bindNullInd _ = 0
>   bindType _ = oci_SQLT_INT

> instance OracleBind Double where
>   bindWithValue v a = withBinaryValue toCDouble v (\p v -> poke (castPtr p) v) a
>   bindSize _ = (sizeOf (toCDouble 0.0))
>   bindNullInd _ = 0
>   bindType _ = oci_SQLT_FLT

> instance OracleBind CalendarTime where
>   bindWithValue v a = withBinaryValue id v (\p dt -> calTimeToBuffer (castPtr p) dt) a
>   bindSize _ = 7
>   bindNullInd _ = 0
>   bindType _ = oci_SQLT_DAT

> instance OracleBind UTCTime where
>   bindWithValue v a = withBinaryValue id v (\p dt -> utcTimeToBuffer (castPtr p) dt) a
>   bindSize _ = 7
>   bindNullInd _ = 0
>   bindType _ = oci_SQLT_DAT

> withBinaryValue :: (OracleBind b) =>
>   (b -> a)  -- ^ convert Haskell value to C value
>   -> b      -- ^ value to convert (we call bindSize on this value to get buffer size)
>   -> (Ptr Word8 -> a -> IO ())  -- ^ action to place converted value into buffer
>   -> (Ptr Word8 -> IO ())       -- ^ action to run over buffer; buffer will be freed on completion
>   -> IO ()
> withBinaryValue fn v pok action =
>   allocaBytes (bindSize v) $ \p -> do
>   pok p (fn v)
>   action (castPtr p)

> clength = fromIntegral . length

We make some possibly invalid assumptions here,
like a c int = 32 bits, c short = 16 bits, c long = 64 bits.

> toCInt :: Int -> CInt; toCInt = fromIntegral
> fromCInt :: CInt -> Int; fromCInt = fromIntegral
> toCInt16 :: Int16 -> CShort; toCInt16 = fromIntegral
> fromCInt16 :: CShort -> Int16; fromCInt16 = fromIntegral
> toCInt32 :: Int32 -> CInt; toCInt32 = fromIntegral
> fromCInt32 :: CInt -> Int32; fromCInt32 = fromIntegral
> toCInt64 :: Int64 -> CLLong; toCInt64 = fromIntegral
> fromCInt64 :: CLLong -> Int64; fromCInt64 = fromIntegral
> toCChar :: Char -> CChar; toCChar = toEnum . fromEnum
> fromCChar :: CChar -> Char; fromCChar = toEnum . fromEnum
> toCDouble :: Double -> CDouble; toCDouble = realToFrac
> fromCDouble :: CDouble -> Double; fromCDouble = realToFrac
> toCFloat :: Float -> CFloat; toCFloat = realToFrac
> fromCFloat :: CFloat -> Float; fromCFloat = realToFrac



20040822073512
   10000000000 (10 ^ 10) * year
     100000000 (10 ^ 8) * month
       1000000 (10 ^ 6) * day
         10000  (10^4) * hour

Use quot and rem, /not/ div and mod,
so that we get sensible behaviour for -ve numbers.

> makeCalTime :: Int64 -> CalendarTime
> makeCalTime i =
>   let
>     year = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in CalendarTime
>     { ctYear = fromIntegral year
>     , ctMonth = toEnum (fromIntegral month - 1)
>     , ctDay = fromIntegral day
>     , ctHour = fromIntegral hour
>     , ctMin = fromIntegral minute
>     , ctSec = fromIntegral second
>     , ctPicosec = 0
>     , ctWDay = Sunday
>     , ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }

> makeInt64 :: CalendarTime -> Int64
> makeInt64 ct =
>   let
>     yearm :: Int64
>     yearm = 10000000000
>   in  yearm * fromIntegral (ctYear ct)
>   + 100000000 * fromIntegral ((fromEnum (ctMonth ct) + 1))
>   + 1000000 * fromIntegral (ctDay ct)
>   + 10000 * fromIntegral (ctHour ct)
>   + 100 * fromIntegral (ctMin ct)
>   + fromIntegral (ctSec ct)


--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

We save a reference to the parent PreparedStmt.
In a lot of cases (the simple ones) the parent is that same
as the PreparedStmt.
It only differs wen we use the NextResultSet instance of makeQuery.

> data Query = Query
>   { queryStmt :: PreparedStmt
>   , querySess :: Session
>   , queryParent :: Maybe PreparedStmt
>   }


> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 100  -- sensible default?

> data StmtBind = StmtBind String [BindA Session PreparedStmt BindObj]
> data QueryStringTuned = QueryStringTuned QueryResourceUsage String [BindA Session PreparedStmt BindObj]

> sqlbind :: String -> [BindA Session PreparedStmt BindObj] -> QueryStringTuned
> sqlbind sql parms = QueryStringTuned defaultResourceUsage sql parms

> prefetch :: Int -> String -> [BindA Session PreparedStmt BindObj] -> QueryStringTuned
> prefetch count sql parms = QueryStringTuned (QueryResourceUsage count) sql parms

> instance Statement BoundStmt Session Query where
>   makeQuery sess bstmt = return (Query (boundStmt bstmt) sess (Just (boundStmt bstmt)))

> instance Statement PreparedStmt Session Query where
>   makeQuery sess pstmt = return (Query pstmt sess (Just pstmt))

> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = makeQuery sess sqltext

> instance Statement String Session Query where
>   makeQuery sess sqltext = makeQuery sess (StmtBind sqltext [])

> instance Statement (RefCursor StmtHandle) Session Query where
>   makeQuery sess (RefCursor stmt) = do
>     pstmt <- newPreparedStmt PreparedStmtFreeWithQuery sess stmt
>     return (Query pstmt sess Nothing)

> instance Statement (NextResultSet mark PreparedStmt) Session Query where
>   makeQuery sess (NextResultSet (Enum.PreparedStmt pstmt)) = do
>     cursors <- readIORef (stmtCursors pstmt)
>     if null cursors then throwDB (DBError ("02", "000") (-1) "No more result sets to process.") else return ()
>     writeIORef (stmtCursors pstmt) (tail cursors)
>     makeQuery sess (head cursors)

> instance Statement StmtBind Session Query where
>   makeQuery sess (StmtBind sqltext bas) =
>     makeQuery sess (QueryStringTuned defaultResourceUsage sqltext bas)

> instance Statement QueryStringTuned Session Query where
>   makeQuery sess (QueryStringTuned resUsage sqltext bas) = do
>     let (PreparationA action) = prepareStmt' sqltext PreparedStmtFreeWithQuery
>     pstmt <- action sess
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     setPrefetchCount sess (stmtHandle pstmt) (prefetchRowCount resUsage)
>     execute sess (stmtHandle pstmt) 0
>     return (Query pstmt sess (Just pstmt))


> data ColumnBuffer = ColumnBuffer 
>    { bufferFPtr :: OCI.ColumnResultBuffer
>    , nullIndFPtr :: ForeignPtr CShort
>    , retSizeFPtr :: ForeignPtr CShort
>    , bufSize :: Int
>    , colPos :: Int
>    , bufType :: CInt
>    }

> instance IQuery Query Session ColumnBuffer where
>   destroyQuery query =
>     case (queryStmt query) of
>       PreparedStmtFreeWithQuery stmth sess _ _ -> closeStmt sess stmth
>       _ -> return ()
>   fetchOneRow query = do
>     rc <- fetchRow (querySess query) (queryStmt query)
>     return (rc /= oci_NO_DATA)
>   currentRowNum query =
>     getRowCount (querySess query) (stmtHandle (queryStmt query))
>   freeBuffer q buffer = return ()


> nullIf :: Bool -> a -> Maybe a
> nullIf test v = if test then Nothing else Just v


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

> allocBuffer query (bufsize, ociBufferType) colpos = do
>     (_, buf, nullptr, sizeptr) <- liftIO $ defineCol (querySess query) (queryStmt query) colpos bufsize ociBufferType
>     return $ ColumnBuffer
>       { bufferFPtr = buf
>       , nullIndFPtr = nullptr
>       , retSizeFPtr = sizeptr
>       , bufSize = bufsize
>       , colPos = colpos
>       , bufType = ociBufferType
>       }


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

> bufferToUTCTime :: ColumnBuffer -> IO (Maybe UTCTime)
> bufferToUTCTime colbuf = maybeBufferNull colbuf Nothing $
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
>     let year = makeYear century100 year100
>     return (Database.Enumerator.mkUTCTime year month day (hour-1) (minute-1) (second-1))

> setBufferByte :: OCI.BufferPtr -> Int -> Word8 -> IO ()
> setBufferByte buf n v =
>   pokeByteOff buf n v

> calTimeToBuffer :: OCI.BufferPtr -> CalendarTime -> IO ()
> calTimeToBuffer buf ct = do
>   setBufferByte buf 0 (makeCentByte (ctYear ct))
>   setBufferByte buf 1 (makeYearByte (ctYear ct))
>   setBufferByte buf 2 (fromIntegral ((fromEnum (ctMonth ct)) + 1))
>   setBufferByte buf 3 (fromIntegral (ctDay ct))
>   setBufferByte buf 4 (fromIntegral (ctHour ct + 1))
>   setBufferByte buf 5 (fromIntegral (ctMin ct + 1))
>   setBufferByte buf 6 (fromIntegral (ctSec ct + 1))

> utcTimeToBuffer :: OCI.BufferPtr -> UTCTime -> IO ()
> utcTimeToBuffer buf utc = do
>   let (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
>   let (TimeOfDay hour minute second) = time
>   let (year, month, day) = toGregorian ltday
>   setBufferByte buf 0 (makeCentByte (fromIntegral year))
>   setBufferByte buf 1 (makeYearByte (fromIntegral year))
>   setBufferByte buf 2 (fromIntegral month)
>   setBufferByte buf 3 (fromIntegral day)
>   setBufferByte buf 4 (fromIntegral (hour+1))
>   setBufferByte buf 5 (fromIntegral (minute+1))
>   setBufferByte buf 6 (round (second+1))


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

> bufferToStmtHandle :: ColumnBuffer -> IO (RefCursor StmtHandle)
> bufferToStmtHandle buffer = do
>   withForeignPtr (bufferFPtr buffer) $ \bufferPtr -> do
>     v <- peek (castPtr bufferPtr)
>     return (RefCursor v)


> instance DBType (RefCursor StmtHandle) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (8, oci_SQLT_RSET) n
>   fetchCol q buffer = do
>     rc <- bufferToStmtHandle buffer
>     appendRefCursor q rc
>     return rc

> appendRefCursor query stmth = do
>   case queryParent query of
>     -- no parent stmt => probably just a doQuery over a RefCursor.
>     -- Don't bother saving returned RefCursors.
>     Nothing -> return ()
>     Just pstmt -> modifyIORef (stmtCursors pstmt) (++ [stmth])


> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (16000, oci_SQLT_CHR) n
>   fetchCol q buffer = bufferToString buffer

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (4, oci_SQLT_INT) n
>   fetchCol q buffer = do
>     outputBuffers <- readIORef (stmtBuffers (queryStmt q))
>     if null outputBuffers then bufferToInt buffer
>       else outputBufferToInt (outputBuffers !! ((colPos buffer) - 1))

FIXME  implement output buffers...

> outputBufferToInt buffer = return undefined

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (8, oci_SQLT_FLT) n
>   fetchCol q buffer = bufferToDouble buffer

> instance DBType (Maybe UTCTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (7, oci_SQLT_DAT) n
>   fetchCol q buffer = bufferToUTCTime buffer

> instance DBType (Maybe CalendarTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (7, oci_SQLT_DAT) n
>   fetchCol q buffer = bufferToDatetime buffer

|This single polymorphic instance covers all of the
type-specific non-Maybe instances e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor v q n = allocBufferFor (Just v) q n
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) (fetchCol q buffer)

> buffer_pos q buffer = do
>   row <- currentRowNum q
>   return (row,colPos buffer)


|A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (16000, oci_SQLT_CHR) n
>   fetchCol q buffer = do
>     v <- bufferToString buffer
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing
