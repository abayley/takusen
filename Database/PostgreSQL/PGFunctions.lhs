
|
Module      :  Database.PostgreSQL.PGFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple wrappers for PostgreSQL functions (FFI) plus middle-level
wrappers (in the second part of this file)

> {-# OPTIONS -ffi #-}
> {-# OPTIONS -fglasgow-exts #-}

> module Database.PostgreSQL.PGFunctions where

> import Foreign
> import Foreign.C
> import Foreign.Ptr
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Data.Int
> import Data.Time
> import System.IO


> data DBHandleStruct = PGconn
> type DBHandle = Ptr DBHandleStruct
> data StmtStruct = PGresult
> type ResultSetHandle = Ptr StmtStruct
> type Oid = CUInt -- from postgres_ext.h
> type Format = CInt
> type Void = ()
> type ParamLen = CInt
> invalidOid = 0
> -- type Blob = Ptr Word8

> data PGException = PGException Int String
>   deriving (Typeable)

> instance Show PGException where
>   show (PGException i s) = "PGException " ++ (show i) ++ " " ++ s

> catchPG :: IO a -> (PGException -> IO a) -> IO a
> catchPG = catchDyn

> throwPG :: Integral a => a -> String -> any
> throwPG rc s = throwDyn (PGException (fromIntegral rc) s)

> rethrowPG :: PGException -> any
> rethrowPG = throwDyn

> cStr :: CStringLen -> CString
> cStr = fst
> cStrLen :: CStringLen -> CInt
> cStrLen = fromIntegral . snd


PG sends binary data in big-endian byte-order (really?),
at least according to the manual page for the DECLARE command
(in the Reference section of the manual).

The example C programs in the manual use the htonl/ntohl functions
to reverse the byte order before-sending/after-receiving (resp.)
I've included the decls here for reference, but we now use a
Haskell function reverseBytes to achieve the same result.
Not sure if just reversing the byte-order is enough/correct though;
I could have been lucky due to the use of a Wintel platform.


include C:\MinGW\include\winsock.h

 foreign import stdcall "winsock.h htonl" htonl :: Word32 -> Word32
 foreign import stdcall "winsock.h ntohl" ntohl :: Word32 -> Word32

> foreign import ccall "libpq-fe.h PQconnectdb" fPQconnectdb
>   :: CString -> IO DBHandle

> foreign import ccall "libpq-fe.h PQfinish" fPQfinish
>   :: DBHandle -> IO ()

From the PostgreSQL 8.0.0 docs, postgresql/libpq.html:

    This function will close the connection to the server and attempt to
    reestablish a new connection to the same server, using all the same
    parameters previously used. This may be useful for error recovery if a
    working connection is lost.

> foreign import ccall "libpq-fe.h PQreset" fPQreset
>   :: DBHandle -> IO ()

Returns the database name of the connection
This is guaranteed by the Postgresql documentation to be a pure function

> foreign import ccall "libpq-fe.h PQdb" fPQdb
>   :: DBHandle -> CString

Inquire the connection status

> type ConnStatusType = CInt -- enumeration, see libpq-fe.h
> [eCONNECTION_OK,eCONNECTION_BAD] = [0..1]::[ConnStatusType]
> foreign import ccall "libpq-fe.h PQstatus" fPQstatus
>   :: DBHandle -> IO ConnStatusType

Returns the error message most recently generated by an operation on the
connection.

> foreign import ccall "libpq-fe.h PQerrorMessage" fPQerrorMessage
>   :: DBHandle -> IO CString


28.10. Notice Processing
We have a couple of callback hooks for server notice messages.

The C decls are:
  typedef void (*PQnoticeReceiver) (void *arg, const PGresult *res);
  PQnoticeReceiver PQsetNoticeReceiver(PGconn *conn, PQnoticeReceiver proc, void *arg);

  typedef void (*PQnoticeProcessor) (void *arg, const char *message);
  PQnoticeProcessor PQsetNoticeProcessor(PGconn *conn, PQnoticeProcessor proc, void *arg);

> type NoticeReceiver = Ptr () -> ResultSetHandle -> IO ()
> type NoticeProcessor = Ptr () -> CString -> IO ()

We need the wrapper to turn our NoticeReceiver-typed function
into something libpq can consume.
This decl will make GHC create the wrapper function for us.
GHC generates files PGFunctions_stub.h and PGFunctions_stub.c.

The wrapper decl syntax is odd, as it's more an export than an import...

> foreign import ccall "wrapper" mkNoticeReceiver ::
>   NoticeReceiver -> IO (FunPtr NoticeReceiver)
> foreign import ccall "wrapper" mkNoticeProcessor ::
>   NoticeProcessor -> IO (FunPtr NoticeProcessor)

> foreign import ccall "libpq-fe.h PQsetNoticeReceiver" fPQsetNoticeReceiver
>   :: DBHandle -> FunPtr NoticeReceiver -> Ptr () -> IO (FunPtr NoticeReceiver)
> foreign import ccall "libpq-fe.h PQsetNoticeProcessor" fPQsetNoticeProcessor
>   :: DBHandle -> FunPtr NoticeProcessor -> Ptr () -> IO (FunPtr NoticeProcessor)



Execution of commands

We don't use fPQexec; the docs suggest fPQexecParams is better anyway
(better defense from SQL injection attacks, etc).

 foreign import ccall "libpq-fe.h PQexec" fPQexec
   :: DBHandle -> CString -> IO ResultSetHandle

> foreign import ccall "libpq-fe.h PQexecParams" fPQexecParams
>   :: DBHandle -> CString -> CInt -> Ptr Oid -> Ptr Void -> Ptr ParamLen -> Ptr Format ->
>      CInt -> IO ResultSetHandle

> foreign import ccall "libpq-fe.h PQprepare" fPQprepare
>   :: DBHandle -> CString -> CString -> CInt -> Ptr Oid -> IO ResultSetHandle

> foreign import ccall "libpq-fe.h PQexecPrepared" fPQexecPrepared
>   :: DBHandle -> CString -> CInt -> Ptr Void -> Ptr ParamLen -> Ptr Format ->
>      CInt -> IO ResultSetHandle



> foreign import ccall "libpq-fe.h PQresultStatus" fPQresultStatus
>   :: ResultSetHandle -> IO ExecStatusType

> type ExecStatusType = CInt -- enumeration, see libpq-fe.h
> (ePGRES_EMPTY_QUERY : 
>  ePGRES_COMMAND_OK : 
>  ePGRES_TUPLES_OK :
>  ePGRES_COPY_OUT : 
>  ePGRES_COPY_IN : 
>  ePGRES_BAD_RESPONSE :
>  ePGRES_NONFATAL_ERROR : 
>  ePGRES_FATAL_ERROR : 
>  _)
>  = [0..] :: [ExecStatusType]

> (textResultSet:binaryResultSet:_) = [0,1] :: [CInt]
> (xxx_textParameters:binaryParameters:_) = [0,1] :: [CInt]

> foreign import ccall "libpq-fe.h PQresultErrorMessage" fPQresultErrorMessage
>   :: ResultSetHandle -> IO CString

> foreign import ccall "libpq-fe.h PQclear" fPQclear
>   :: ResultSetHandle -> IO ()


Retrieving Query Result Information

> foreign import ccall "libpq-fe.h PQntuples" fPQntuples
>   :: ResultSetHandle -> IO CInt
> foreign import ccall "libpq-fe.h PQnfields" fPQnfields
>   :: ResultSetHandle -> IO CInt

Inquiry about a column; Column numbers start at 0

> foreign import ccall "libpq-fe.h PQfname" fPQfname
>   :: ResultSetHandle -> CInt -> IO CString
> foreign import ccall "libpq-fe.h PQfformat" fPQfformat
>   :: ResultSetHandle -> CInt -> IO CInt
> foreign import ccall "libpq-fe.h PQftype" fPQftype
>   :: ResultSetHandle -> CInt -> IO Oid

Really getting the values

> foreign import ccall "libpq-fe.h PQgetvalue" fPQgetvalue
>   :: ResultSetHandle -> CInt -> CInt -> IO (Ptr Word8)
> foreign import ccall "libpq-fe.h PQgetisnull" fPQgetisnull
>   :: ResultSetHandle -> CInt -> CInt -> IO CInt
> foreign import ccall "libpq-fe.h PQgetlength" fPQgetlength
>   :: ResultSetHandle -> CInt -> CInt -> IO CInt

27.3.3. Retrieving Result Information for Other Commands

> foreign import ccall "libpq-fe.h PQcmdStatus" fPQcmdStatus
>   :: ResultSetHandle -> IO CString
> foreign import ccall "libpq-fe.h PQcmdTuples" fPQcmdTuples
>   :: ResultSetHandle -> IO CString
> foreign import ccall "libpq-fe.h PQoidValue" fPQoidValue
>   :: ResultSetHandle -> IO Oid

27.8. Functions Associated with the COPY Command

> foreign import ccall "libpq-fe.h PQputCopyData" fPQputCopyData
>   :: DBHandle -> Ptr Word8 -> CInt -> IO CInt
> foreign import ccall "libpq-fe.h PQputCopyEnd" fPQputCopyEnd
>   :: DBHandle -> CString -> IO CInt
> foreign import ccall "libpq-fe.h PQgetResult" fPQgetResult
>   :: DBHandle -> IO ResultSetHandle


27.9. Control Functions

> type PGVerbosity = CInt -- enumeration, see libpq-fe.h
> (ePQERRORS_TERSE : 
>  ePQERRORS_DEFAULT :
>  ePQERRORS_VERBOSE :
>  _) = [0..]::[PGVerbosity]
> foreign import ccall "libpq-fe.h PQsetErrorVerbosity" fPQsetErrorVerbosity
>   :: DBHandle -> PGVerbosity -> IO PGVerbosity



-------------------------------------------------------------------
            Middle-level interface

Get the current error message

> getError :: DBHandle -> IO String
> getError db = fPQerrorMessage db >>= peekCString

conn'parm is a string with all the attributes

> openDb :: String -> IO DBHandle
> openDb conn'parm =
>   withCString conn'parm $ \cstr -> do
>   db <- fPQconnectdb cstr
>   if db == nullPtr
>     then throwPG (-1) "Null PGconn handle from PQconnectdb"
>     else do
>     rc <- fPQstatus db
>     if rc == eCONNECTION_OK
>       then return db
>       else do
>         emsg <- getError db
>         fPQfinish db
>         throwPG rc emsg

> closeDb :: DBHandle -> IO ()
> closeDb db = fPQfinish db


> ignoreNotices _ _ = return ()
> reportNotices _ cstr = peekCString cstr >>= hPutStrLn stderr

> disableNoticeReporting db = do
>   r <- mkNoticeProcessor ignoreNotices
>   fPQsetNoticeProcessor db r nullPtr

> enableNoticeReporting db = do
>   r <- mkNoticeProcessor reportNotices
>   fPQsetNoticeProcessor db r nullPtr

> setErrorVerbosity db verb = fPQsetErrorVerbosity db verb >> return ()

-----------------------------------------------------------

Here we define a class useful in marshalling
Haskell values to and from their Postgres counterparts
(for binding and defining).

TypeOID is a class to convert types to their OIDs.
We need this for the prepare functions,
which take an array of OIDs to indicate parameter types.
We can find the OIDs with this query:
select typname, oid from pg_type

PG timestamps are stored as 8-byte Doubles (i.e. double-precision
floating point) holding the seconds before or after midnight 2000-01-01.
timestamp with time zone = oid 1184  (8 bytes)
timestamp without time zone = oid 1114  (8 bytes)
time = oid 1083  (8 bytes)
timetz = oid 1266  (12 bytes)
date = oid 1082  (4 bytes)

> class PGType a where
>   pgTypeFormat :: a -> Format
>   pgTypeOid :: a -> Oid
>   pgNewValue :: a -> IO (Ptr Word8)
>   pgPeek :: Ptr Word8 -> IO a
>   pgSize :: a -> Int
>   -- default impls
>   pgTypeFormat _ = 1

> instance PGType a => PGType (Maybe a) where
>   pgTypeFormat Nothing = pgTypeFormat (undefined::a)
>   pgTypeFormat (Just v) = pgTypeFormat v
>   pgTypeOid Nothing = pgTypeOid (undefined::a)
>   pgTypeOid (Just v) = pgTypeOid v
>   pgNewValue Nothing = return nullPtr
>   pgNewValue (Just v) = pgNewValue v
>   pgPeek ptr = if ptr == nullPtr then return Nothing else pgPeek ptr >>= return . Just
>   pgSize Nothing = 0  -- what is the size of a null value?... probably irrelevant
>   pgSize (Just v) = pgSize v

> pgZeroDate :: UTCTime
> pgZeroDate = UTCTime (fromGregorian 2000 1 1) 0

> toPGTime :: UTCTime -> Double
> toPGTime date = realToFrac (diffUTCTime date pgZeroDate)

> fromPGTime :: Double -> UTCTime
> fromPGTime secs = addUTCTime (realToFrac secs) pgZeroDate


timestamp and timestamp with time zone are probably exactly the same
in terms of internal representation, so it's really just an input-output
semantics that the two types distinguish.

> instance PGType UTCTime where
>   pgTypeOid _ = 1114
>   pgNewValue v = newBinaryValue toCDouble (toPGTime v)
>   pgPeek p = peekValueRev undefined fromCDouble p >>= return . fromPGTime
>   pgSize _ = (sizeOf (toCDouble 0.0))


> instance PGType String where
>   pgTypeFormat _ = 0
>   pgTypeOid _ = 25
>   pgNewValue s = newCString s >>= return . castPtr
>   pgPeek = peekCString . castPtr
>   pgSize s = length s

> instance PGType Char where
>   pgTypeOid _ = 18
>   pgNewValue v = newBinaryValue toCChar v
>   pgPeek p = peek (castPtr p) >>= return . fromCChar
>   pgSize _ = (sizeOf (toCChar 'a'))

> instance PGType Int where
>   pgTypeOid _ = 23
>   pgNewValue v = newBinaryValue toCInt v
>   pgPeek p = peekValueRev undefined fromCInt p
>   pgSize _ = (sizeOf (toCInt 0))

> instance PGType Int16 where
>   pgTypeOid _ = 21
>   pgNewValue v = newBinaryValue toCInt16 v
>   pgPeek p = peekValueRev undefined fromCInt16 p
>   pgSize _ = (sizeOf (toCInt16 0))

> instance PGType Int32 where
>   pgTypeOid _ = 23
>   pgNewValue v = newBinaryValue toCInt32 v
>   pgPeek p = peekValueRev undefined fromCInt32 p
>   pgSize _ = (sizeOf (toCInt32 0))

> instance PGType Int64 where
>   pgTypeOid _ = 20
>   pgNewValue v = newBinaryValue toCInt64 v
>   pgPeek p = peekValueRev undefined fromCInt64 p
>   pgSize _ = (sizeOf (toCInt64 0))

> instance PGType Double where
>   pgTypeOid _ = 701
>   pgNewValue v = newBinaryValue toCDouble v
>   pgPeek p = peekValueRev undefined fromCDouble p
>   pgSize _ = (sizeOf (toCDouble 0.0))

> instance PGType Float where
>   pgTypeOid _ = 700
>   pgNewValue v = newBinaryValue toCFloat v
>   pgPeek p = peekValueRev undefined fromCFloat p
>   pgSize _ = (sizeOf (toCFloat 0.0))

> data PGBindVal = PGBindVal
>   { bindValOid :: Oid
>   , bindValFormat :: Format
>   , bindValSize :: CInt
>   , bindValPtr :: IO (Ptr Word8)
>   }

newBindVal is useful when creating lists of bind values,
for passing to the stmtExec and prepare'n'exec functions.

> newBindVal v = PGBindVal (pgTypeOid v) (pgTypeFormat v) (toCInt (pgSize v)) (pgNewValue v)

> bindTypes vs = map bindValOid vs

Binary values are sent in network byte order,
which means we must reverse the byte order before sending,
and after receiving.

> newBinaryValue :: (Storable a, PGType b) => (b -> a) -> b -> IO (Ptr Word8)
> newBinaryValue fn v = do
>   let v2 = fn v
>   let sz = pgSize v
>   alloca $ \p -> do
>   poke p v2
>   p2 <- mallocBytes sz
>   reverseBytes sz (castPtr p) p2
>   return p2

> peekValueRev :: (Storable a, PGType b) => b -> (a -> b) -> Ptr Word8 -> IO b
> peekValueRev v fn fromptr = do
>   let sz = pgSize v
>   allocaArray sz $ \toptr -> do
>     reverseBytes sz (castPtr fromptr) (castPtr toptr)
>     peek toptr >>= return . fn

> reverseBytes :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
> reverseBytes n buffrom bufto = reverseBytes' n buffrom (plusPtr bufto (n-1))

> reverseBytes' :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
> reverseBytes' 0 _ _ = return ()
> reverseBytes' n buffrom bufto = do
>   b <- peek buffrom
>   poke bufto b
>   reverseBytes' (n-1) (plusPtr buffrom 1) (plusPtr bufto (-1))


Fer debuggin'.

> printBytes i n ptr = do
>   if i >= n then putStr "\n"
>     else do
>       b <- peekByteOff ptr i :: IO Word8
>       putStr (' ':(show b))
>       printBytes (i+1) n ptr


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


-----------------------------------------------------------

Check the ResultSetHandle returned by fPQexec and similar functions

> check'stmt :: DBHandle -> ExecStatusType -> ResultSetHandle -> IO ResultSetHandle
> check'stmt db _ stmt
>   | stmt == nullPtr = do -- something is really wrong
>       emsg <- getError db 
>       rc <- fPQstatus db
>       throwPG rc emsg
> check'stmt _ expected'status stmt =  do
>   rc <- fPQresultStatus stmt
>   if rc == expected'status then return stmt
>     else do
>       msg <- fPQresultErrorMessage stmt >>= peekCString
>       fPQclear stmt
>       throwPG rc msg

> stmtPrepare :: DBHandle -> String -> String -> [Oid] -> IO String
> stmtPrepare db stmt'name sqlText types =
>   withCString stmt'name $ \csn -> do
>   withCString sqlText $ \cstr -> do
>   withArray types $ \ctypearray -> do
>     let np = fromIntegral $ length types
>     stmt <- fPQprepare db csn cstr np ctypearray
>     check'stmt db ePGRES_COMMAND_OK stmt
>     fPQclear stmt            -- doesn't have any useful info
>     return stmt'name


Execute some kind of statement that returns no tuples.
Because this is a frequently used function, we code it specially
(rather than invoking a more generic execCommand).

> nqExec :: DBHandle -> String -> IO (String, String, Oid)
> nqExec db sqlText =
>   withCString sqlText $ \cstr -> 
>     do
>     stmt <- fPQexecParams db cstr 0 nullPtr nullPtr nullPtr nullPtr 0
>             >>= check'stmt db ePGRES_COMMAND_OK
>     -- save all information from PGresult and free it
>     cmd'status  <- fPQcmdStatus stmt >>= peekCString
>     cmd'ntuples <- fPQcmdTuples stmt >>= peekCString
>     cmd'oid     <- fPQoidValue stmt
>     fPQclear stmt
>     return (cmd'status, cmd'ntuples, cmd'oid)

> execCommand :: DBHandle -> String -> [PGBindVal] -> IO (String, String, Oid)
> execCommand db sqlText bindvals = do
>   stmtPrepare db "" sqlText (bindTypes bindvals)
>   execPreparedCommand db "" bindvals

> execPreparedCommand :: DBHandle -> String -> [PGBindVal] -> IO (String, String, Oid)
> execPreparedCommand db stmtname bindvals = do
>   (rs, ntuples) <- execPrepared db stmtname bindvals ePGRES_COMMAND_OK
>   -- save all information from PGresult and free it
>   cmd'status  <- fPQcmdStatus rs >>= peekCString
>   cmd'ntuples <- fPQcmdTuples rs >>= peekCString
>   cmd'oid     <- fPQoidValue rs
>   stmtFinalise rs
>   return (cmd'status, cmd'ntuples, cmd'oid)

Prepare and Execute a query. Returns results as binary.

> stmtExecImm :: DBHandle -> String -> [PGBindVal] -> IO (ResultSetHandle, Int)
> stmtExecImm db sqlText bindvals = do
>   let np = fromIntegral $ length bindvals
>   withCString sqlText $ \cstr -> do 
>   withArray (map bindValOid bindvals) $ \coidarray -> do
>   withArray (map bindValSize bindvals) $ \clenarray -> do
>   withArray (map bindValFormat bindvals) $ \cformatarray -> do
>   -- The bindValPtrs are IO actions; executing them (via sequence)
>   -- creates the bind values (allocates storage, pokes values, etc).
>   -- We must remember to free these later.
>   paramlist <- sequence (map bindValPtr bindvals)
>   withArray paramlist $ \cparamarray -> do
>     rs <- fPQexecParams db cstr np coidarray (castPtr cparamarray) clenarray cformatarray binaryResultSet
>     mapM_ (\p -> if p == nullPtr then return () else free p) paramlist
>     check'stmt db ePGRES_TUPLES_OK rs
>     ntuples <- fPQntuples rs
>     return (rs, fromIntegral ntuples)


A simple version with no binding parameters and returning results as text

> stmtExecImm0 :: DBHandle -> String -> IO (ResultSetHandle, Int)
> stmtExecImm0 db sqlText =
>   withCString sqlText $ \cstr -> do 
>     rs <- fPQexecParams db cstr 0 nullPtr nullPtr nullPtr nullPtr 0
>     check'stmt db ePGRES_TUPLES_OK rs
>     ntuples <- fPQntuples rs
>     return (rs, fromIntegral ntuples)


Execute a previously prepared query, with no params.

> stmtExec0 :: DBHandle -> String -> IO (ResultSetHandle, Int)
> stmtExec0 db stmt'name = stmtExec0bt db stmt'name binaryResultSet

> stmtExec0t :: DBHandle -> String -> IO (ResultSetHandle, Int)
> stmtExec0t db stmt'name = stmtExec0bt db stmt'name textResultSet

> stmtExec0bt db stmt'name binary_or_text = 
>   withCString stmt'name $ \cstmtname -> do
>     rs <- fPQexecPrepared db cstmtname 0 nullPtr nullPtr nullPtr
>                           binary_or_text
>     check'stmt db ePGRES_TUPLES_OK rs
>     ntuples <- fPQntuples rs
>     return (rs, fromIntegral ntuples)


Execute a previously prepared query, with params.

> stmtExec :: DBHandle -> String -> [PGBindVal] -> IO (ResultSetHandle, Int)
> stmtExec db stmt'name bindvals = execPrepared db stmt'name bindvals ePGRES_TUPLES_OK

This is used for both queries and commands.
We have to pass the expected PQresult code, because
queries return ePGRES_TUPLES_OK while commands return ePGRES_COMMAND_OK.

> execPrepared :: DBHandle -> String -> [PGBindVal] -> CInt -> IO (ResultSetHandle, Int)
> execPrepared db stmt'name bindvals rc = do
>   let np = fromIntegral $ length bindvals
>   withCString stmt'name $ \cstmtname -> do
>   withArray (map bindValSize bindvals) $ \clenarray -> do
>   withArray (map bindValFormat bindvals) $ \cformatarray -> do
>   -- The bindValPtrs are IO actions; executing them (via sequence)
>   -- creates the bind values (allocates storage, pokes values, etc).
>   -- We must remember to free these later.
>   paramlist <- sequence (map bindValPtr bindvals)
>   withArray paramlist $ \cparamarray -> do
>     rs <- fPQexecPrepared db cstmtname np (castPtr cparamarray) clenarray cformatarray binaryResultSet
>     mapM_ (\p -> if p == nullPtr then return () else free p) paramlist
>     check'stmt db rc rs
>     ntuples <- fPQntuples rs
>     return (rs, fromIntegral ntuples)

> prepare'n'exec :: DBHandle -> String -> String -> [PGBindVal] -> IO (ResultSetHandle, Int)
> prepare'n'exec db stmtname stmt bindvals = do
>   let np = fromIntegral $ length bindvals
>   withCString stmtname $ \cstmtname -> do
>   withCString stmt $ \cstmt -> do
>   withArray (map bindValOid bindvals) $ \coidarray -> do
>     rs <- fPQprepare db cstmtname cstmt np coidarray
>     check'stmt db ePGRES_COMMAND_OK rs
>     execPrepared db stmtname bindvals ePGRES_TUPLES_OK


Free storage, that is. No error in Postgres

> stmtFinalise :: ResultSetHandle -> IO ()
> stmtFinalise = fPQclear

|Column numbers are zero-indexed, so subtract one
from given index (we present a one-indexed interface).
So are the row numbers.

> colValPtr :: ResultSetHandle -> Int -> Int -> IO (Ptr Word8)
> colValPtr rs row col =
>   fPQgetvalue rs (toCInt (row-1)) (toCInt (col-1)) >>= return . castPtr

> colValBinary rs row col = colValPtr rs row col >>= pgPeek

Test the result-set column to see if it's in text or binary format.
If text, then use read to parse it into the desired value.

> colVal :: (Read a, PGType a) => ResultSetHandle -> Int -> Int -> IO a
> colVal rs row col = do
>   n <- fPQntuples rs
>   if (fromIntegral n) < row || row < 1
>     then throwPG (-1) ("Attempted fetch from invalid row number " ++ show row)
>     else do
>       fmt <- fPQfformat rs (toCInt (col-1))
>       if fmt == 0
>         then colValBinary rs row col >>= return . read
>         else colValBinary rs row col

Don't want to use read to parse string values;
they're strings already.

> colValString :: ResultSetHandle -> Int -> Int -> IO String
> colValString = colValBinary

> colValInt :: ResultSetHandle -> Int -> Int -> IO Int
> colValInt = colVal

> colValInt64 :: ResultSetHandle -> Int -> Int -> IO Int64
> colValInt64 = colVal

> colValDouble :: ResultSetHandle -> Int -> Int -> IO Double
> colValDouble = colVal

> colValFloat :: ResultSetHandle -> Int -> Int -> IO Float
> colValFloat = colVal

> colValUTCTime :: ResultSetHandle -> Int -> Int -> IO UTCTime
> colValUTCTime = colValBinary


> colValNull :: ResultSetHandle -> Int -> Int -> IO Bool
> colValNull rs row col = do
>   ind <- fPQgetisnull rs (toCInt (row-1)) (toCInt (col-1))
>   return (ind /= 0)

> {-
> colValBlob :: ResultSetHandle -> Int -> IO (ForeignPtr Blob)
> colValBlob rs stmt colnum = do
>   let ccolnum = fromIntegral (colnum - 1)
>   bytes <- sqliteColumnBytes stmt ccolnum
>   src <- sqliteColumnBlob stmt ccolnum
>   buffer <- mallocForeignPtrBytes bytes
>   withForeignPtr buffer $ \dest -> copyBytes dest src bytes
>   return (castForeignPtr buffer)
> -}

> substituteBindPlaceHolders sql = sbph sql 1 False ""

> sbph :: String -> Int -> Bool -> String -> String
> sbph [] _ _ acc = reverse acc
> sbph ('\'':cs) i inQuote acc = sbph cs i (not inQuote) ('\'':acc)
> sbph ('?':cs) i False acc = sbph cs (i+1) False ((reverse (show i)) ++ ('$':acc))
> sbph (c:cs) i inQuote acc = sbph cs i inQuote (c:acc)


Execute the COPY FROM STDIN command

> nqCopyIn :: DBHandle -> String -> Handle -> IO ()
> nqCopyIn_buflen :: Int = 8192
> nqCopyIn db sqlText hin =
>   withCString sqlText $ \cstr -> 
>    allocaBytes nqCopyIn_buflen $ \buffer ->
>     do
>     stmt <- fPQexecParams db cstr 0 nullPtr nullPtr nullPtr nullPtr 0
>             >>= check'stmt db ePGRES_COPY_IN
>     let check'copy'status 1 = return ()
>         check'copy'status _ = do
>		                emsg <- getError db 
>                               rc <- fPQstatus db
>                               throwPG rc emsg
>     let loop = do
>	   len <- hGetBuf hin buffer nqCopyIn_buflen
>          if len < 0 then withCString "IO error" $ fPQputCopyEnd db
>             else if len == 0 then fPQputCopyEnd db nullPtr
>             else fPQputCopyData db buffer (fromIntegral len) >>=
>	           check'copy'status >> loop
>     res <- loop
>     fPQclear stmt
>     check'copy'status res
>     -- finishing, checking the final status
>     fPQgetResult db >>= check'stmt db ePGRES_COMMAND_OK >>= fPQclear
