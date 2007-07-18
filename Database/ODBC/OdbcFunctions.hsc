
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif
#include <sql.h>
#include <sqlext.h>

#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

{-
http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/odbcodbc_api_reference.asp
http://www.dbmaker.com.tw/reference/manuals/odbc/odbc_chap_04.html
-}

module Database.ODBC.OdbcFunctions where

import Control.Exception
import Control.Monad
import Data.Dynamic
import Data.Time
import Database.Util
import Foreign
import Foreign.C
import Foreign.C.UTF8

data HandleObj = HandleObj
type Handle = Ptr HandleObj
data EnvObj = EnvObj
type EnvHandle = Ptr EnvObj
data ConnObj = ConnObj
type ConnHandle = Ptr ConnObj
data StmtObj = StmtObj
type StmtHandle = Ptr StmtObj
type WindowHandle = Ptr ()
data Buffer = Buffer
type BufferFPtr = ForeignPtr Buffer
type SizeFPtr = ForeignPtr SqlLen

newtype BindBuffer = BindBuffer (BufferFPtr, SizeFPtr)

{-
   Copied from HDBC's ODBC helper code.
   SQL_OV_ODBC3 expands to 3, but we can't cast a numeric literal 3
   into a pointer in Haskell. C has no such compunctions, though.
-}
-- #def void *getSqlOvOdbc3(void) {  return (void *)3UL; }
#def void *getSqlOvOdbc3(void) {  return (void *)SQL_OV_ODBC3; }
 
type SqlInteger = #{type SQLINTEGER}
type SqlUInteger = #{type SQLUINTEGER}
type SqlSmallInt = #{type SQLSMALLINT}
type SqlUSmallInt = #{type SQLUSMALLINT}
type SqlLen = #{type SQLLEN}
type SqlULen = #{type SQLULEN}
type SqlReturn = SqlSmallInt
type SqlHandleType = SqlSmallInt
type SqlDataType = SqlSmallInt
type SqlCDataType = SqlSmallInt
type SqlParamDirection = SqlSmallInt

-- Return codes from API calls

(
  sqlRcInvalidHandle :
  sqlRcStillExecuting :
  sqlRcSuccess :
  sqlRcSuccessWithInfo :
  sqlRcError :
  sqlRcNeedData :
  sqlRcNoData :
--  []) = [-2,-1,0,1,2,99,100] :: [SqlReturn]
  []) =
  (
  #{const SQL_INVALID_HANDLE} :
  #{const SQL_STILL_EXECUTING} :
  #{const SQL_SUCCESS} :
  #{const SQL_SUCCESS_WITH_INFO} :
  #{const SQL_ERROR} :
  #{const SQL_NEED_DATA} :
  #{const SQL_NO_DATA} :
  []) :: [SqlReturn]

showReturnCode rc
  | rc == #{const SQL_INVALID_HANDLE} = "SQL_INVALID_HANDLE"
  | rc == #{const SQL_STILL_EXECUTING} = "SQL_STILL_EXECUTING"
  | rc == #{const SQL_SUCCESS} = "SQL_SUCCESS"
  | rc == #{const SQL_SUCCESS_WITH_INFO}  = "SQL_SUCCESS_WITH_INFO"
  | rc == #{const SQL_ERROR} = "SQL_ERROR"
  | rc == #{const SQL_NEED_DATA} = "SQL_NEED_DATA"
  | rc == #{const SQL_NO_DATA} = "SQL_NO_DATA"
  | otherwise = "UNKNOWN_RETURN_CODE"

-- There are only four handle types in ODBC.

(
  sqlHTypeEnv :
  sqlHTypeConn :
  sqlHTypeStmt :
  sqlHTypeDesc :
--  []) = [1..4] :: [SqlHandleType]
  []) =
  (
  #{const SQL_HANDLE_ENV} :
  #{const SQL_HANDLE_DBC} :
  #{const SQL_HANDLE_STMT} :
  #{const SQL_HANDLE_DESC} :
  []) :: [SqlHandleType]

sqlDriverNoPrompt :: SqlUSmallInt
sqlDriverNoPrompt = #{const SQL_DRIVER_NOPROMPT}

sqlAttrOdbcVersion :: SqlInteger
sqlAttrOdbcVersion = #{const SQL_ATTR_ODBC_VERSION}

sqlNullTermedString :: SqlInteger
sqlNullTermedString = #{const SQL_NTS}

sqlNullData :: SqlLen
sqlNullData = #{const SQL_NULL_DATA}

-- ODBC SQL data types
(
  sqlDTypeString :
  sqlDTypeInt :
  sqlDTypeBinary :
  sqlDTypeDouble :
  sqlDTypeDate :
  sqlDTypeTime :
  sqlDTypeTimestamp :
  []) =
  (
  #{const SQL_CHAR} :
  #{const SQL_INTEGER} :
  #{const SQL_BINARY} :
  #{const SQL_DOUBLE} :
  #{const SQL_DATE} :
  #{const SQL_TIME} :
  #{const SQL_TIMESTAMP} :
  []) :: [SqlDataType]

-- host language (C) data types

(
  sqlCTypeString :
  sqlCTypeInt :
  sqlCTypeBinary :
  sqlCTypeDouble :
  sqlCTypeDate :
  sqlCTypeTime :
  sqlCTypeTimestamp :
  []) =
  (
  #{const SQL_C_CHAR} :
  #{const SQL_C_LONG} :
  #{const SQL_C_BINARY} :
  #{const SQL_C_DOUBLE} :
  #{const SQL_C_DATE} :
  #{const SQL_C_TIME} :
  #{const SQL_C_TIMESTAMP} :
  []) :: [SqlCDataType]

{-
#define SQL_C_BINARY SQL_BINARY
#define SQL_C_BIT SQL_BIT
#define SQL_C_BOOKMARK SQL_C_ULONG
#define SQL_C_CHAR SQL_CHAR
#define SQL_C_DATE SQL_DATE
#define SQL_C_DEFAULT 99
#define SQL_C_DOUBLE SQL_DOUBLE
#define SQL_C_FLOAT SQL_REAL
#define SQL_C_LONG SQL_INTEGER
#define SQL_C_SHORT SQL_SMALLINT
#define SQL_C_SLONG (SQL_C_LONG+SQL_SIGNED_OFFSET)
#define SQL_C_SSHORT (SQL_C_SHORT+SQL_SIGNED_OFFSET)
#define SQL_C_STINYINT (SQL_TINYINT+SQL_SIGNED_OFFSET)
#define SQL_C_TIME SQL_TIME
#define SQL_C_TIMESTAMP SQL_TIMESTAMP
#define SQL_C_TINYINT SQL_TINYINT
#define SQL_C_ULONG (SQL_C_LONG+SQL_UNSIGNED_OFFSET)
#define SQL_C_USHORT (SQL_C_SHORT+SQL_UNSIGNED_OFFSET)
#define SQL_C_UTINYINT (SQL_TINYINT+SQL_UNSIGNED_OFFSET)
-}

(
  sqlParamInput :
  sqlParamInputOutput :
  sqlParamOutput :
  sqlParamDefault :
  sqlParamUnknown :
  [] ) =
  (
  #{const SQL_PARAM_INPUT} :
  #{const SQL_PARAM_INPUT_OUTPUT} :
  #{const SQL_PARAM_OUTPUT} :
  #{const SQL_PARAM_TYPE_DEFAULT} :
  #{const SQL_PARAM_TYPE_UNKNOWN} :
  [] ) :: [SqlParamDirection]


data OdbcException = OdbcException Int String String [OdbcException]
  deriving (Typeable)

instance Show OdbcException where
  show (OdbcException i st s _) = "OdbcException "
    ++ (show i) ++ " " ++ st ++ " " ++ s

catchOdbc :: IO a -> (OdbcException -> IO a) -> IO a
catchOdbc = catchDyn

throwOdbc :: OdbcException -> a
throwOdbc = throwDyn

-- #define SQL_SUCCEEDED(rc) (((rc)&(~1))==0)
sqlSucceeded rc = rc == sqlRcSuccess || rc == sqlRcSuccessWithInfo

type MyCString = CString
type MyCStringLen = CStringLen
myPeekCStringLen p = peekUTF8StringLen p
myWithCString s = withUTF8String s
myWithCStringLen s = withUTF8StringLen s
--myPeekCStringLen p = peekCStringLen p
--myWithCString s = withCString s
--myWithCStringLen s = withCStringLen s

getDiagRec :: SqlReturn -> SqlHandleType -> Handle -> SqlSmallInt -> IO [OdbcException]
getDiagRec retcode htype handle row =
  allocaBytes 6 $ \cstrState -> do
  alloca $ \errorNumPtr -> do
  allocaBytes 1025 $ \cstrMsg -> do
  alloca $ \msgLenPtr -> do
    rc <- sqlGetDiagRec htype handle row cstrState errorNumPtr cstrMsg 1024 msgLenPtr
    --putStrLn ("getDiagRec: rc=" ++ show rc)
    case () of
      _ | rc == sqlRcSuccess -> do
          errnum <- peek errorNumPtr
          state <- myPeekCStringLen (cstrState, 5)
          msglen <- peek msgLenPtr
          --putStrLn ("getDiagRec: msglen=" ++ show msglen)
          msg <- myPeekCStringLen (cstrMsg, fromIntegral msglen)
          --putStrLn ("getDiagRec: msg=" ++ msg)
          more <- getDiagRec retcode htype handle (row+1)
          return ((OdbcException (fromIntegral errnum) state msg []) : more)
        | rc == sqlRcNoData -> return []
        | otherwise -> return [OdbcException (fromIntegral rc) "01000" (showReturnCode rc) []]

checkError :: SqlReturn -> SqlHandleType -> Handle -> IO ()
checkError rc htype handle =
  when (rc /= sqlRcSuccess)
    (do
      exs <- getDiagRec rc htype handle 1
      if null exs
        then throwOdbc (OdbcException (fromIntegral rc) "01000"
          ("No error messages for return code " ++ show rc ++ " (" ++ showReturnCode rc ++ ")") [])
        else do
          let (OdbcException i st s _) = head exs
          throwOdbc (OdbcException i st s (tail exs))
    )

allocHdl :: (Storable a) => Handle -> SqlHandleType -> IO a
allocHdl h htype = do
  alloca $ \hptr -> do
    rc <- sqlAllocHandle htype h (castPtr hptr)
    checkError rc htype h
    peek hptr

allocEnv :: IO EnvHandle
allocEnv = allocHdl nullPtr sqlHTypeEnv

allocConn :: EnvHandle -> IO ConnHandle
allocConn env = allocHdl (castPtr env) sqlHTypeConn

allocStmt :: ConnHandle -> IO StmtHandle
allocStmt conn = allocHdl (castPtr conn) sqlHTypeStmt

freeHelper :: SqlHandleType -> Handle -> IO ()
freeHelper htype h = do
  rc <- sqlFreeHandle htype h
  checkError rc htype h

freeEnv :: EnvHandle -> IO ()
freeEnv env = freeHelper sqlHTypeEnv (castPtr env)

freeConn :: ConnHandle -> IO ()
freeConn conn = freeHelper sqlHTypeConn (castPtr conn)

freeStmt :: StmtHandle -> IO ()
freeStmt stmt = freeHelper sqlHTypeStmt (castPtr stmt)

setOdbcVer :: EnvHandle -> IO ()
setOdbcVer env = do
  rc <- sqlSetEnvAttr env sqlAttrOdbcVersion getSqlOvOdbc3 0
  checkError rc sqlHTypeEnv (castPtr env)

connect :: ConnHandle -> String -> IO String
connect conn connstr = do
  myWithCStringLen connstr $ \(cstr, clen) -> do
  allocaBytes 1000 $ \outConnStr -> do
  alloca $ \ptrOutLen -> do
  rc <- sqlDriverConnect conn nullPtr cstr (fromIntegral clen)
    outConnStr 1000 ptrOutLen sqlDriverNoPrompt
  checkError rc sqlHTypeConn (castPtr conn)
  outLen <- peek ptrOutLen
  myPeekCStringLen (outConnStr, fromIntegral outLen)

disconnect :: ConnHandle -> IO ()
disconnect conn = do
  rc <- sqlDisconnect conn
  checkError rc sqlHTypeConn (castPtr conn)


prepareStmt :: StmtHandle -> String -> IO ()
prepareStmt stmt sqltext = do
  myWithCString sqltext $ \cstr -> do
  rc <- sqlPrepare stmt cstr sqlNullTermedString
  checkError rc sqlHTypeStmt (castPtr stmt)

executeStmt :: StmtHandle -> IO ()
executeStmt stmt = do
  rc <- sqlExecute stmt
  checkError rc sqlHTypeStmt (castPtr stmt)

closeCursor :: StmtHandle -> IO ()
closeCursor stmt = do
  rc <- sqlCloseCursor stmt
  checkError rc sqlHTypeStmt (castPtr stmt)

rowCount :: StmtHandle -> IO Int
rowCount stmt = do
  alloca $ \rcptr -> do
  rc <- sqlRowCount stmt rcptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  liftM fromIntegral (peek rcptr)

-- | Return 'True' if there are more rows, 'False' if end-of-data.
fetch :: StmtHandle -> IO Bool
fetch stmt = do
  rc <- sqlFetch stmt
  when (rc /= sqlRcNoData)
    (checkError rc sqlHTypeStmt (castPtr stmt))
  return (rc /= sqlRcNoData)


---------------------------------------------------------------------
-- Get column values with SQLGetData

getMaybeFromBuffer :: Storable a => Ptr SqlLen -> Ptr a -> (Ptr a -> SqlLen -> IO b) -> IO (Maybe b)
getMaybeFromBuffer szptr bptr action = do
  len <- peek szptr
  if len < 0 then return Nothing
    else action bptr len >>= return . Just

getDataStorable :: Storable a => StmtHandle -> Int -> SqlDataType -> Int -> (a -> b) -> IO (Maybe b)
getDataStorable stmt pos sqltype buffersize convert = do
  allocaBytes buffersize $ \bptr -> do
  alloca $ \szptr -> do
  rc <- sqlGetData stmt (fromIntegral pos) sqltype (castPtr bptr) 0 szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  getMaybeFromBuffer szptr bptr (\buffer len -> peek buffer >>= return . convert )

getDataInt :: StmtHandle -> Int -> IO (Maybe Int)
getDataInt stmt pos = getDataStorable stmt pos sqlDTypeInt (sizeOf cint) convert
  where convert :: CInt -> Int; convert = fromIntegral
        cint :: CInt; cint = 0

getDataDouble :: StmtHandle -> Int -> IO (Maybe Double)
getDataDouble stmt pos = getDataStorable stmt pos sqlDTypeDouble (sizeOf cdbl) convert
  where convert :: CDouble -> Double; convert = realToFrac
        cdbl :: CDouble; cdbl = 0

getDataUtcTime :: StmtHandle -> Int -> IO (Maybe UTCTime)
getDataUtcTime stmt pos = do
  allocaBytes 50 $ \bptr -> do
  alloca $ \szptr -> do
  rc <- sqlGetData stmt (fromIntegral pos) sqlDTypeTimestamp (castPtr bptr) 50 szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  getMaybeFromBuffer szptr bptr (\buffer len -> getUtcTimeFromBuffer buffer >>= return )

getDataUTF8String :: StmtHandle -> Int -> IO (Maybe String)
getDataUTF8String stmt pos = do
  allocaBytes 100000 $ \bptr -> do
  alloca $ \szptr -> do
  rc <- sqlGetData stmt (fromIntegral pos) sqlDTypeString (castPtr bptr) 100000 szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  getMaybeFromBuffer szptr bptr (\buffer len -> peekUTF8StringLen (buffer, fromIntegral len) >>= return )

{- from sqltypes.h. Struct size depends on size of SmallInt etc,
but probably 16 bytes on a 32-bit platform. 32 bytes on 64-bit?
typedef struct tagTIMESTAMP_STRUCT {
    SQLSMALLINT year;
    SQLUSMALLINT month;
    SQLUSMALLINT day;
    SQLUSMALLINT hour;
    SQLUSMALLINT minute;
    SQLUSMALLINT second;
    SQLUINTEGER fraction;
} TIMESTAMP_STRUCT;
-}

peekSmallInt :: Ptr a -> Int -> IO SqlSmallInt
peekSmallInt buffer offset = peekByteOff buffer offset
peekUSmallInt :: Ptr a -> Int -> IO SqlUSmallInt
peekUSmallInt buffer offset = peekByteOff buffer offset
peekUInteger :: Ptr a -> Int -> IO SqlUInteger
peekUInteger buffer offset = peekByteOff buffer offset

-- We have to give the Ptr a concrete type, to keep the type-checker
-- happy, but it can be anything. It has to be a Storable type, though.

getUtcTimeFromBuffer :: Ptr Int -> IO UTCTime
getUtcTimeFromBuffer buffer = do
  year <- peekSmallInt buffer #{offset TIMESTAMP_STRUCT, year}
  month <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, month}
  day <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, day}
  hour <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, hour}
  minute <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, minute}
  second <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, second}
  -- what do we do with fraction? What sort of fraction is it?
  -- millisecs? microsecs? picosecs?
  fraction <- peekUInteger buffer #{offset TIMESTAMP_STRUCT, fraction}
  return (mkUTCTime (fromIntegral year) month day hour minute second)

---------------------------------------------------------------------
-- Return-set column binding.
-- Apparently this is faster than SQLGetData for larger result-sets,
-- because the output buffers do not need to be rebound with
-- every call.
-- Dunno how much difference this'll make in practice. Suck 'n' see.

bindColBuffer :: StmtHandle -> Int -> SqlDataType -> Int -> IO BindBuffer
bindColBuffer stmt pos dtype size = do
  bfptr <- mallocForeignPtrBytes size
  szfptr <- mallocForeignPtr
  withForeignPtr bfptr $ \bptr -> do
  withForeignPtr szfptr $ \szptr -> do
  rc <- sqlBindCol stmt (fromIntegral pos) dtype bptr (fromIntegral size) szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  return (BindBuffer (bfptr, szfptr))


testForNull :: BindBuffer -> (Ptr Buffer -> SqlLen -> IO a) -> IO (Maybe a)
testForNull (BindBuffer (bfptr, szfptr)) action = do
  withForeignPtr szfptr $ \szptr -> do
  len <- peek szptr
  -- sqlNullData = -1, so anthing less than zero could
  -- be treated as null.
  if len < 0 then return Nothing
    else withForeignPtr bfptr $ \bptr ->
      action bptr len >>= return . Just
  
getStorableFromBuffer :: Storable a => BindBuffer -> IO (Maybe a)
getStorableFromBuffer buffer =
  testForNull buffer (\bptr _ -> peek (castPtr bptr))

getIntFromBuffer :: BindBuffer -> IO (Maybe Int)
getIntFromBuffer buffer = getStorableFromBuffer buffer

getDoubleFromBuffer :: BindBuffer -> IO (Maybe Double)
getDoubleFromBuffer buffer = getStorableFromBuffer buffer

getCAStringFromBuffer :: BindBuffer -> IO (Maybe String)
getCAStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekCAStringLen (castPtr ptr, fromIntegral len))

getCWStringFromBuffer :: BindBuffer -> IO (Maybe String)
getCWStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekCWStringLen (castPtr ptr, fromIntegral len))

getUTF8StringFromBuffer :: BindBuffer -> IO (Maybe String)
getUTF8StringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekUTF8StringLen (castPtr ptr, fromIntegral len))

getUtcTimeFromBindBuffer :: BindBuffer -> IO (Maybe UTCTime)
getUtcTimeFromBindBuffer bindbuffer = do
  testForNull bindbuffer $ \buffer _ -> do
  getUtcTimeFromBuffer (castPtr buffer)


---------------------------------------------------------------------
-- Parameter binding

createBufferForStorable :: Storable a => Maybe a -> IO BindBuffer
createBufferForStorable Nothing =
  let zero :: Int; zero = 0; in createBufferHelper zero (-1)
createBufferForStorable (Just val) = createBufferHelper val (fromIntegral (sizeOf val))

createBufferHelper :: Storable a => a -> SqlLen -> IO BindBuffer
createBufferHelper val size = do
  szfptr <- mallocForeignPtr
  withForeignPtr szfptr (\szptr -> poke szptr size)
  bfptr <- mallocForeignPtr
  withForeignPtr bfptr (\bptr -> poke bptr val)
  return (BindBuffer (castForeignPtr bfptr, szfptr))

wrapSizedBuffer :: Ptr a -> SqlLen -> IO BindBuffer
wrapSizedBuffer valptr size = do
  szfptr <- mallocForeignPtr
  withForeignPtr szfptr (\szptr -> poke szptr size)
  bfptr <- newForeignPtr finalizerFree valptr
  return (BindBuffer (castForeignPtr bfptr, szfptr))

bindParam :: StmtHandle -> Int -> SqlCDataType -> SqlDataType -> BindBuffer -> IO ()
bindParam stmt pos ctype sqltype (BindBuffer (bfptr, szfptr)) =
  withForeignPtr bfptr $ \bptr -> do
  withForeignPtr szfptr $ \szptr -> do
  size <- peek szptr
  rc <- sqlBindParameter stmt (fromIntegral pos) sqlParamInput ctype sqltype 0 0 bptr size szptr
  checkError rc sqlHTypeStmt (castPtr stmt)

bindNull :: StmtHandle -> Int -> SqlCDataType -> SqlDataType -> IO BindBuffer
bindNull stmt pos ctype dtype = do
  let val :: Maybe Int; val = Nothing
  buffer <- createBufferForStorable val
  bindParam stmt pos ctype dtype buffer
  return buffer

bindParamInt :: StmtHandle -> Int -> Maybe Int -> IO BindBuffer
bindParamInt stmt pos val = do
  buffer <- createBufferForStorable val
  bindParam stmt pos sqlCTypeInt sqlDTypeInt buffer
  return buffer

bindParamDouble :: StmtHandle -> Int -> Maybe Double -> IO BindBuffer
bindParamDouble stmt pos val = do
  buffer <- createBufferForStorable val
  bindParam stmt pos sqlCTypeDouble sqlDTypeDouble buffer
  return buffer

bindParamCString :: StmtHandle -> Int -> Maybe CStringLen -> IO BindBuffer
bindParamCString stmt pos Nothing =
  bindNull stmt pos sqlCTypeString sqlDTypeString
bindParamCString stmt pos (Just (cstr, clen)) = do
  --putStrLn ("bindParamCString: clen = " ++ show clen)
  buffer <- wrapSizedBuffer cstr (fromIntegral clen)
  bindParam stmt pos sqlCTypeString sqlDTypeString buffer
  return buffer

bindEncodedString :: StmtHandle -> Int -> Maybe String -> (String -> ((Ptr a, Int) -> IO BindBuffer) -> IO BindBuffer) -> IO BindBuffer
bindEncodedString stmt pos Nothing withEncoder =
  bindNull stmt pos sqlCTypeString sqlDTypeString
bindEncodedString stmt pos (Just s) withEncoder =
  withEncoder s (\(cs, cl) -> bindParamCString stmt pos (Just (castPtr cs, cl)))

bindParamUTF8String :: StmtHandle -> Int -> Maybe String -> IO BindBuffer
bindParamUTF8String stmt pos val =
  bindEncodedString stmt pos val withUTF8StringLen

bindParamCAString :: StmtHandle -> Int -> Maybe String -> IO BindBuffer
bindParamCAString stmt pos val =
  bindEncodedString stmt pos val withCAStringLen

bindParamCWString :: StmtHandle -> Int -> Maybe String -> IO BindBuffer
bindParamCWString stmt pos val =
  bindEncodedString stmt pos val withCWStringLen

pokeSmallInt :: Ptr a -> Int -> SqlSmallInt -> IO ()
pokeSmallInt buffer offset val = pokeByteOff buffer offset val
pokeUSmallInt :: Ptr a -> Int -> SqlUSmallInt -> IO ()
pokeUSmallInt buffer offset val = pokeByteOff buffer offset val
pokeUInteger :: Ptr a -> Int -> SqlUInteger -> IO ()
pokeUInteger buffer offset val = pokeByteOff buffer offset val

bindParamUtcTime :: StmtHandle -> Int -> Maybe UTCTime -> IO BindBuffer
bindParamUtcTime stmt pos Nothing = do
  bindNull stmt pos sqlCTypeTimestamp sqlDTypeTimestamp
bindParamUtcTime stmt pos (Just utc) = do
  let
    (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
    (TimeOfDay hour minute second) = time
    (year, month, day) = toGregorian ltday
  buffer <- mallocBytes 33
  pokeSmallInt buffer #{offset TIMESTAMP_STRUCT, year} (fromIntegral year)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, month} (fromIntegral month)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, day} (fromIntegral day)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, hour} (fromIntegral hour)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, minute} (fromIntegral minute)
  let secs :: SqlUSmallInt; secs = round second
  let fraction :: SqlUInteger; fraction = 0
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, second} secs
  -- what do we do with fraction? What sort of fraction is it?
  -- millisecs? microsecs? picosecs?
  --pokeUInteger buffer #{offset TIMESTAMP_STRUCT, fraction} fraction
  buffer <- wrapSizedBuffer buffer (fromIntegral (sizeOf fraction) + #{offset TIMESTAMP_STRUCT, fraction})
  bindParam stmt pos sqlCTypeTimestamp sqlDTypeTimestamp buffer
  return buffer

---------------------------------------------------------------------
-- FFI

foreign import #{CALLCONV} unsafe "OdbcFunctions_hsc.h getSqlOvOdbc3" getSqlOvOdbc3 :: Ptr ()

-- From sql.h:
-- SQLRETURN SQL_API SQLAllocHandle(SQLSMALLINT,SQLHANDLE,SQLHANDLE*);
foreign import #{CALLCONV} unsafe "sql.h SQLAllocHandle" sqlAllocHandle ::
  SqlHandleType -> Handle -> Ptr Handle -> IO SqlReturn

-- SQLRETURN SQL_API SQLFreeHandle(SQLSMALLINT,SQLHANDLE);
foreign import #{CALLCONV} unsafe "sql.h SQLFreeHandle" sqlFreeHandle ::
  SqlSmallInt -> Handle -> IO SqlReturn

-- SQLRETURN SQL_API SQLSetEnvAttr(SQLHENV,SQLINTEGER,SQLPOINTER,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLSetEnvAttr" sqlSetEnvAttr ::
  EnvHandle -> SqlInteger -> Ptr () -> SqlInteger -> IO SqlReturn

-- SQLRETURN SQL_API SQLGetDiagRec(SQLSMALLINT,SQLHANDLE,SQLSMALLINT,SQLCHAR*,SQLINTEGER*,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*); 
foreign import #{CALLCONV} unsafe "sql.h SQLGetDiagRec" sqlGetDiagRec ::
  SqlHandleType -> Handle
  -> SqlSmallInt  -- row/message number
  -> MyCString  -- OUT: state
  -> Ptr SqlInteger  -- OUT: error number
  -> MyCString  -- OUT: error message
  -> SqlSmallInt  -- IN: message buffer size
  -> Ptr SqlSmallInt  -- OUT: message length
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLDriverConnect(SQLHDBC,SQLHWND,SQLCHAR*,SQLSMALLINT,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*,SQLUSMALLINT);
foreign import #{CALLCONV} unsafe "sql.h SQLDriverConnect" sqlDriverConnect ::
  ConnHandle -> WindowHandle -> MyCString -> SqlSmallInt ->
  MyCString -> SqlSmallInt -> Ptr SqlSmallInt -> SqlUSmallInt -> IO SqlReturn

-- SQLRETURN SQL_API SQLDisconnect(SQLHDBC);
foreign import #{CALLCONV} unsafe "sql.h SQLDisconnect" sqlDisconnect ::
  ConnHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLPrepare(SQLHSTMT,SQLCHAR*,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLPrepare" sqlPrepare ::
  StmtHandle -> MyCString -> SqlInteger -> IO SqlReturn

-- SQLRETURN SQL_API SQLExecute(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLExecute" sqlExecute ::
  StmtHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLCloseCursor(SQLHSTMT); 
foreign import #{CALLCONV} unsafe "sql.h SQLCloseCursor" sqlCloseCursor ::
  StmtHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLRowCount(SQLHSTMT,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLRowCount" sqlRowCount ::
  StmtHandle -> Ptr SqlLen -> IO SqlReturn

-- SQLRETURN SQL_API SQLGetData(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLGetData" sqlGetData ::
  StmtHandle
  -> SqlUSmallInt  -- column position, 1-indexed
  -> SqlDataType  -- SQL data type: string, int, long, date, etc
  -> Ptr Buffer  -- output buffer
  -> SqlLen  -- output buffer size
  -> Ptr SqlLen -- output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindCol(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindCol" sqlBindCol ::
  StmtHandle
  -> SqlUSmallInt  -- column position, 1-indexed
  -> SqlDataType  -- SQL data type: string, int, long, date, etc
  -> Ptr Buffer  -- output buffer
  -> SqlLen  -- output buffer size
  -> Ptr SqlLen -- output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLFetch(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLFetch" sqlFetch ::
  StmtHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindParameter(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLSMALLINT,SQLSMALLINT,SQLULEN,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindParameter" sqlBindParameter ::
  StmtHandle
  -> SqlUSmallInt  -- position, 1-indexed
  -> SqlParamDirection  -- direction: IN, OUT
  -> SqlCDataType  -- C data type: char, int, long, float, etc
  -> SqlDataType  -- SQL data type: string, int, long, date, etc
  -> SqlULen  -- col size 
  -> SqlSmallInt  -- decimal digits
  -> Ptr Buffer  -- input+output buffer
  -> SqlLen  -- buffer size
  -> Ptr SqlLen -- input+output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn
