
{-# OPTIONS -ffi #-}
{-# OPTIONS -fglasgow-exts #-}
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


-- |
-- Module      :  Database.ODBC.OdbcFunctions
-- Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  oleg@pobox.com, alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Wrappers for ODBC FFI functions, plus buffer marshaling.

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

data BindBuffer = BindBuffer
  { bindBufPtr :: BufferFPtr
  , bindBufSzPtr :: SizeFPtr
  , bindBufSize :: SqlLen
  }

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

sqlNullTermedString :: SqlInteger
sqlNullTermedString = #{const SQL_NTS}

sqlNullData :: SqlLen
sqlNullData = #{const SQL_NULL_DATA}

sqlTransCommit :: SqlSmallInt
sqlTransCommit = #{const SQL_COMMIT}

sqlTransRollback :: SqlSmallInt
sqlTransRollback = #{const SQL_ROLLBACK}

sqlAutoCommitOn, sqlAutoCommitOff :: SqlInteger
sqlAutoCommitOn = #{const SQL_AUTOCOMMIT_ON}
sqlAutoCommitOff = #{const SQL_AUTOCOMMIT_OFF}

-- These are attribute types, which are passed as the second parameter
-- to sqlSetEnvAttr.

(
  sqlAttrOdbcVersion :
  sqlAttrAutoCommit :
  sqlAttrTxnIsolation :
  []) =
  (
  #{const SQL_ATTR_ODBC_VERSION} :
  #{const SQL_ATTR_AUTOCOMMIT} :
  #{const SQL_ATTR_TXN_ISOLATION} :
  []) :: [SqlInteger]

-- These are attribute values, which are passed as the third parameter
-- to sqlSetEnvAttr. Obviously that must accompany the relevant
-- attribute type.

(
  sqlOvOdbc3 :
  sqlTxnCapable :
  sqlDefaultTxnIsolation :
  sqlTxnIsolationOption :
  sqlTxnReadUncommitted :
  sqlTxnReadCommitted :
  sqlTxnRepeatableRead :
  sqlTxnSerializable :
  []) =
  (
  #{const SQL_OV_ODBC3} : -- 3 (UL)
  #{const SQL_TXN_CAPABLE} :  -- 46
  #{const SQL_DEFAULT_TXN_ISOLATION} :  -- 26
  #{const SQL_TXN_ISOLATION_OPTION} :  -- 72
  #{const SQL_TXN_READ_UNCOMMITTED} :  -- 1
  #{const SQL_TXN_READ_COMMITTED} :  -- 2
  #{const SQL_TXN_REPEATABLE_READ} :  -- 4
  #{const SQL_TXN_SERIALIZABLE} :  -- 8
  []) :: [SqlInteger]


-- ODBC SQL data types
(
  sqlDTypeString :
  sqlDTypeInt :
  sqlDTypeBinary :
  sqlDTypeDouble :
  sqlDTypeDate :
  sqlDTypeTime :
  sqlDTypeTimestamp :
  sqlDTypeCursor :
  []) =
  (
  #{const SQL_CHAR} :  -- 1
  #{const SQL_INTEGER} :  -- 4
  #{const SQL_BINARY} :  -- -2
  #{const SQL_DOUBLE} :  -- 8
  #{const SQL_TYPE_DATE} :  -- 9
  #{const SQL_TYPE_TIME} :  -- 10
  #{const SQL_TYPE_TIMESTAMP} :  -- 11
  #{const SQL_CURSOR_TYPE} :  -- 6
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
  #{const SQL_C_TYPE_DATE} :
  #{const SQL_C_TYPE_TIME} :
  #{const SQL_C_TYPE_TIMESTAMP} :
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

-- define SQL_SUCCEEDED(rc) (((rc)&(~1))==0)
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
        | otherwise -> return [OdbcException (fromIntegral rc) "01000" (showReturnCode retcode) []]

checkError :: SqlReturn -> SqlHandleType -> Handle -> IO ()
checkError rc htype handle =
  when (rc /= sqlRcSuccess && rc /= sqlRcSuccessWithInfo)
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

int2Ptr :: SqlInteger -> Ptr ()
int2Ptr i = plusPtr nullPtr (fromIntegral i)

setOdbcVer :: EnvHandle -> IO ()
setOdbcVer env = do
  rc <- sqlSetEnvAttr env sqlAttrOdbcVersion (int2Ptr sqlOvOdbc3) 0
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

moreResults :: StmtHandle -> IO Bool
moreResults stmt = do
  rc <- sqlMoreResults stmt
  when (rc /= sqlRcNoData)
    (checkError rc sqlHTypeStmt (castPtr stmt))
  return (rc /= sqlRcNoData)


commit :: ConnHandle -> IO ()
commit conn = do
  rc <- sqlEndTran sqlHTypeConn (castPtr conn) sqlTransCommit
  checkError rc sqlHTypeConn (castPtr conn)

rollback :: ConnHandle -> IO ()
rollback conn = do
  rc <- sqlEndTran sqlHTypeConn (castPtr conn) sqlTransRollback
  checkError rc sqlHTypeConn (castPtr conn)

setAutoCommitOn :: ConnHandle -> IO ()
setAutoCommitOn conn = do
  rc <- sqlSetConnectAttr conn sqlAttrAutoCommit (int2Ptr sqlAutoCommitOn) 0
  checkError rc sqlHTypeConn (castPtr conn)

setAutoCommitOff :: ConnHandle -> IO ()
setAutoCommitOff conn = do
  rc <- sqlSetConnectAttr conn sqlAttrAutoCommit (int2Ptr sqlAutoCommitOff) 0
  checkError rc sqlHTypeConn (castPtr conn)

setTxnIsolation :: ConnHandle -> SqlInteger -> IO ()
setTxnIsolation conn level = do
  rc <- sqlSetConnectAttr conn sqlAttrTxnIsolation (int2Ptr level) 0
  checkError rc sqlHTypeConn (castPtr conn)


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

getDataUtcTime :: StmtHandle -> Int -> IO (Maybe UTCTime)
getDataUtcTime stmt pos = do
  allocaBytes #{size TIMESTAMP_STRUCT} $ \bptr -> do
  alloca $ \szptr -> do
  rc <- sqlGetData stmt (fromIntegral pos) sqlDTypeTimestamp (castPtr bptr) 50 szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  getMaybeFromBuffer szptr bptr (\buffer len -> readUtcTimeFromMemory buffer >>= return )

getDataCStringLen :: StmtHandle -> Int -> IO (Maybe CStringLen)
getDataCStringLen stmt pos = do
  alloca $ \szptr -> do
  allocaBytes 16 $ \dummyptr -> do
  -- Call GetData with 0-sized buffer to get size information.
  rc <- sqlGetData stmt (fromIntegral pos) sqlDTypeString (castPtr dummyptr) 0 szptr
  when (rc /= sqlRcSuccessWithInfo)
    (checkError rc sqlHTypeStmt (castPtr stmt))
  bufSize <- peek szptr
  let bufSize' = 1 + if bufSize < 0 then 0 else bufSize
  -- Use size information to allocate perfectly-sized buffer.
  allocaBytes (fromIntegral bufSize') $ \bptr -> do
  rc <- sqlGetData stmt (fromIntegral pos) sqlDTypeString (castPtr bptr) 100000 szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  len <- peek szptr
  if len < 0 then return Nothing
    else return (Just (castPtr bptr, fromIntegral len))

getDataUTF8String :: StmtHandle -> Int -> IO (Maybe String)
getDataUTF8String stmt pos = do
  mbcstrlen <- getDataCStringLen stmt pos
  case mbcstrlen of
    Nothing -> return Nothing
    Just cstrlen -> peekUTF8StringLen cstrlen >>= return . Just

getDataCString :: StmtHandle -> Int -> IO (Maybe String)
getDataCString stmt pos = do
  mbcstrlen <- getDataCStringLen stmt pos
  case mbcstrlen of
    Nothing -> return Nothing
    Just cstrlen -> peekCStringLen cstrlen >>= return . Just

{- from sqltypes.h. Struct size depends on size of SmallInt etc,
but is 16 bytes on a 32-bit platform. 32 bytes on 64-bit?
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

readUtcTimeFromMemory :: Ptr Word8 -> IO UTCTime
readUtcTimeFromMemory buffer = do
  year <- peekSmallInt buffer #{offset TIMESTAMP_STRUCT, year}
  month <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, month}
  day <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, day}
  hour <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, hour}
  minute <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, minute}
  second <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, second}
  -- Fraction ranges from 0 - 999,999,999,
  frac <- peekUInteger buffer #{offset TIMESTAMP_STRUCT, fraction}
  let secs :: Double; secs = fromIntegral second + (fromIntegral frac / 1000000000.0)
  return (mkUTCTime (fromIntegral year) month day hour minute second)

---------------------------------------------------------------------
-- Return-set column binding.
-- Apparently this is faster than SQLGetData for larger result-sets,
-- because the output buffers do not need to be rebound with
-- every call.
-- Dunno how much difference this'll make in practice. Suck 'n' see.

bindColumnBuffer :: StmtHandle -> Int -> SqlDataType -> SqlLen -> IO BindBuffer
bindColumnBuffer stmt pos dtype size = do
  buffer <- createEmptyBuffer (fromIntegral size)
  withForeignPtr (bindBufPtr buffer) $ \bptr -> do
  withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
  rc <- sqlBindCol stmt (fromIntegral pos) dtype bptr size szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  return buffer

createEmptyBuffer :: SqlLen -> IO BindBuffer
createEmptyBuffer size = do
  szfptr <- mallocForeignPtr
  bfptr <- mallocForeignPtrBytes (fromIntegral size)
  return (BindBuffer bfptr szfptr size)


testForNull :: BindBuffer -> (Ptr Buffer -> SqlLen -> IO a) -> IO (Maybe a)
testForNull buffer action = do
  withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
  len <- peek szptr
  -- sqlNullData = -1, so anthing less than zero could
  -- be treated as null.
  if len < 0 then return Nothing
    else withForeignPtr (bindBufPtr buffer) $ \bptr ->
      action bptr len >>= return . Just
  
getStorableFromBuffer :: Storable a => BindBuffer -> IO (Maybe a)
getStorableFromBuffer buffer =
  testForNull buffer (\bptr _ -> peek (castPtr bptr))

getCAStringFromBuffer :: BindBuffer -> IO (Maybe String)
getCAStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekCAStringLen (castPtr ptr, fromIntegral len))

getCWStringFromBuffer :: BindBuffer -> IO (Maybe String)
getCWStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekCWStringLen (castPtr ptr, fromIntegral len))

getUTF8StringFromBuffer :: BindBuffer -> IO (Maybe String)
getUTF8StringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekUTF8StringLen (castPtr ptr, fromIntegral len))

getUtcTimeFromBuffer :: BindBuffer -> IO (Maybe UTCTime)
getUtcTimeFromBuffer bindbuffer = do
  testForNull bindbuffer $ \buffer _ -> do
  readUtcTimeFromMemory (castPtr buffer)


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
  return (BindBuffer (castForeignPtr bfptr) szfptr (fromIntegral size))

wrapSizedBuffer :: Ptr a -> SqlLen -> IO BindBuffer
wrapSizedBuffer valptr size = do
  szfptr <- mallocForeignPtr
  withForeignPtr szfptr (\szptr -> poke szptr size)
  bfptr <- newForeignPtr finalizerFree valptr
  return (BindBuffer (castForeignPtr bfptr) szfptr (fromIntegral size))

bindParam ::
  StmtHandle
  -> Int
  -> SqlParamDirection
  -> SqlCDataType
  -> SqlDataType
  -> SqlULen
  -> SqlSmallInt
  -> BindBuffer
  -> IO ()
bindParam stmt pos direction ctype sqltype precision scale buffer =
  withForeignPtr (bindBufPtr buffer) $ \bptr -> do
  withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
  size <- peek szptr
  rc <- sqlBindParameter stmt (fromIntegral pos) direction ctype sqltype precision scale bptr size szptr
  checkError rc sqlHTypeStmt (castPtr stmt)

-- sqlParamInput
bindNull :: StmtHandle -> Int -> SqlParamDirection -> SqlCDataType -> SqlDataType -> IO BindBuffer
bindNull stmt pos direction ctype dtype = do
  let val :: Maybe Int; val = Nothing
  buffer <- createBufferForStorable val
  bindParam stmt pos direction ctype dtype 0 0 buffer
  return buffer

bindParamCStringLen :: StmtHandle -> Int -> SqlParamDirection -> Maybe CStringLen -> IO BindBuffer
bindParamCStringLen stmt pos direction Nothing =
  bindNull stmt pos direction sqlCTypeString sqlDTypeString
bindParamCStringLen stmt pos direction (Just (cstr, clen)) = do
  buffer <- wrapSizedBuffer cstr (fromIntegral clen)
  bindParam stmt pos direction sqlCTypeString sqlDTypeString (fromIntegral clen) 0 buffer
  return buffer

bindEncodedString :: StmtHandle -> Int -> SqlParamDirection -> Maybe String -> (String -> ((Ptr a, Int) -> IO BindBuffer) -> IO BindBuffer) -> IO BindBuffer
bindEncodedString stmt pos direction Nothing withEncoder =
  bindNull stmt pos direction sqlCTypeString sqlDTypeString
bindEncodedString stmt pos direction (Just s) withEncoder =
  withEncoder s (\(cs, cl) -> bindParamCStringLen stmt pos direction (Just (castPtr cs, cl)))

bindParamUTF8String :: StmtHandle -> Int -> SqlParamDirection -> Maybe String -> IO BindBuffer
bindParamUTF8String stmt pos direction val =
  bindEncodedString stmt pos direction val withUTF8StringLen

bindParamCAString :: StmtHandle -> Int -> SqlParamDirection -> Maybe String -> IO BindBuffer
bindParamCAString stmt pos direction val =
  bindEncodedString stmt pos direction val withCAStringLen

bindParamCWString :: StmtHandle -> Int -> SqlParamDirection -> Maybe String -> IO BindBuffer
bindParamCWString stmt pos direction val =
  bindEncodedString stmt pos direction val withCWStringLen

pokeSmallInt :: Ptr a -> Int -> SqlSmallInt -> IO ()
pokeSmallInt buffer offset val = pokeByteOff buffer offset val
pokeUSmallInt :: Ptr a -> Int -> SqlUSmallInt -> IO ()
pokeUSmallInt buffer offset val = pokeByteOff buffer offset val
pokeUInteger :: Ptr a -> Int -> SqlUInteger -> IO ()
pokeUInteger buffer offset val = pokeByteOff buffer offset val

writeUTCTimeToMemory :: Ptr Word8 -> UTCTime -> IO ()
writeUTCTimeToMemory buffer utc = do
  let
    (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
    (TimeOfDay hour minute second) = time
    (year, month, day) = toGregorian ltday
  pokeSmallInt buffer #{offset TIMESTAMP_STRUCT, year} (fromIntegral year)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, month} (fromIntegral month)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, day} (fromIntegral day)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, hour} (fromIntegral hour)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, minute} (fromIntegral minute)
  -- what do we do with fraction? What sort of fraction is it?
  -- apparently can range from 0 - 999,999,999,
  -- but MS SQL Server only handles milliseconds (0 - 999) i.e. precision 3
  let (secs, frac) = properFraction second
  let fraction :: SqlUInteger; fraction = round (frac * 1000000000.0)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, second} secs
  pokeUInteger buffer #{offset TIMESTAMP_STRUCT, fraction} fraction

-- writeUTCTimeToMemory and makeUtcTimeBuffer don't work with MS SQL Server;
-- it always returns 22008 "Datetime field overflow".
-- They work with PostgreSQL and Oracle ODBC drivers.
-- So I'll leave the code here, in case anyone is desperate
-- to marshal via TIMESTAMP_STRUCT, rather than strings.

makeUtcTimeBuffer :: UTCTime -> IO BindBuffer
makeUtcTimeBuffer utc = do
  mem <- mallocBytes #{size TIMESTAMP_STRUCT}
  writeUTCTimeToMemory (castPtr mem) utc
  wrapSizedBuffer mem #{size TIMESTAMP_STRUCT}

makeUtcTimeStringBuffer :: UTCTime -> IO BindBuffer
makeUtcTimeStringBuffer utc = do
  mem <- mallocBytes 40
  let s = utcTimeToOdbcDatetime utc
  withCStringLen s $ \(cstr, clen) -> do
    copyBytes mem cstr (fromIntegral clen)
    pokeByteOff mem (fromIntegral clen) (0 :: Word8)
    wrapSizedBuffer mem (fromIntegral clen)
  
bindParamUtcTime :: StmtHandle -> Int -> SqlParamDirection -> Maybe UTCTime -> IO BindBuffer
bindParamUtcTime stmt pos direction Nothing = do
  bindNull stmt pos direction sqlCTypeTimestamp sqlDTypeTimestamp
bindParamUtcTime stmt pos direction (Just utc) = do
  -- For TimeStamp:
  --   Size/Length should be 16 bytes.
  --   Precision should be 8 (or 16?).
  --   Scale is the number of digits in the fraction component (SQL Server only allows 0-3).
  -- We're not using the TIMESTAMP_STRUCT to marshal any more.
  --buffer <- makeUtcTimeBuffer utc
  --bindParam stmt pos direction sqlCTypeTimestamp sqlDTypeTimestamp #{size TIMESTAMP_STRUCT} 0 buffer
  --
  -- We have to pass in a String, rather than a TIMESTAMP_STRUCT,
  -- and let the ODBC driver do the conversion.
  -- I cannot get TIMESTAMP_STRUCT to work with MS SQL Server;
  -- it always returns 22008 "Datetime field overflow".
  buffer <- makeUtcTimeStringBuffer utc
  withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
    size <- peek szptr
    -- 23 is the largest precision value allowed by MS SQL Server.
    -- That gives us "yyyy-mm-dd hh:mm:ss.fff"
    bindParam stmt pos direction sqlCTypeString sqlDTypeTimestamp 23 0 buffer
    return buffer


---------------------------------------------------------------------
-- Binding with class...

sizeOfMaybe :: forall a. Storable a => Maybe a -> Int
sizeOfMaybe _ = sizeOf ( undefined :: a )
-- H98 stylee...
--sizeOfMaybe v@Nothing = sizeOfMaybe (asTypeOf (Just undefined) v)
--sizeOfMaybe (Just v) = sizeOf v

newtype OutParam a = OutParam a
newtype InOutParam a = InOutParam a

{-
Separate out the type different types of binding: parameter and column.

Both types use the same BindBuffer, but bind parameters
can send and receive values from the buffer, whereas
column binds only receive values.
This distinction will be handled by having a set of
instances for OdbcBindBuffer that are of the form (Maybe a),
where a is one of the normal database types e.g. Int, Double,
String, UTCTime.
The instances for OdbcBindParam will include the (Maybe a) set,
and also (OutParam (Maybe a)), and (InOutParam (Maybe a)),
to indicate Out and In/Out paramaters.

When we do a column buffer bind, we require a dummy value
of the column data type, so that we know which instance to use.
-}

class OdbcBindBuffer a where
  bindColBuffer
    :: StmtHandle  -- ^ stmt handle
    -> Int  -- ^ column position (1-indexed)
    -> Int  -- ^ size of result buffer (ignored when it can be inferred from type of a)
    -> a  -- ^ dummy value of the appropriate type (just to ensure we get the right class instance)
    -> IO BindBuffer  -- ^ returns a 'BindBuffer' object
  getFromBuffer :: BindBuffer -> IO a
  getData :: StmtHandle -> Int -> IO a

instance OdbcBindBuffer (Maybe Int) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeInt (fromIntegral (sizeOfMaybe val))
  getFromBuffer buffer = getStorableFromBuffer buffer
  getData stmt pos = getDataStorable stmt pos sqlDTypeInt (sizeOf cint) convert
    where convert :: CInt -> Int; convert = fromIntegral
          cint :: CInt; cint = 0

instance OdbcBindBuffer (Maybe Double) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeDouble (fromIntegral (sizeOfMaybe val))
  getFromBuffer buffer = getStorableFromBuffer buffer
  getData stmt pos = getDataStorable stmt pos sqlDTypeDouble (sizeOf cdbl) convert
    where convert :: CDouble -> Double; convert = realToFrac
          cdbl :: CDouble; cdbl = 0

instance OdbcBindBuffer (Maybe String) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeString (fromIntegral size)
  getFromBuffer buffer = getUTF8StringFromBuffer buffer
  getData stmt pos = getDataUTF8String stmt pos

instance OdbcBindBuffer (Maybe UTCTime) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeTimestamp #{size TIMESTAMP_STRUCT}
  getFromBuffer buffer = getUtcTimeFromBuffer buffer
  getData stmt pos = getDataUtcTime stmt pos


class OdbcBindParam a where
  bindParamBuffer
    :: StmtHandle  -- ^ stmt handle
    -> Int  -- ^ parameter position (1-indexed)
    -> a  -- ^ value to write to buffer
    -> IO BindBuffer  -- ^ returns a 'BindBuffer' object

instance OdbcBindParam (Maybe Int) where
  bindParamBuffer stmt pos val = do
    buffer <- createBufferForStorable val
    bindParam stmt pos sqlParamInput sqlCTypeInt sqlDTypeInt 30 0 buffer
    return buffer

instance OdbcBindParam (Maybe Double) where
  bindParamBuffer stmt pos val = do
    buffer <- createBufferForStorable val
    bindParam stmt pos sqlParamInput sqlCTypeDouble sqlDTypeDouble 50 50 buffer
    return buffer

instance OdbcBindParam (Maybe String) where
  bindParamBuffer stmt pos val = bindParamUTF8String stmt pos sqlParamInput val

instance OdbcBindParam (Maybe UTCTime) where
  bindParamBuffer stmt pos val = bindParamUtcTime stmt pos sqlParamInput val


---------------------------------------------------------------------
-- FFI

-- From sql.h:
-- SQLRETURN SQL_API SQLAllocHandle(SQLSMALLINT,SQLHANDLE,SQLHANDLE*);
foreign import #{CALLCONV} unsafe "sql.h SQLAllocHandle" sqlAllocHandle ::
  SqlHandleType -> Handle -> Ptr Handle -> IO SqlReturn

-- SQLRETURN SQL_API SQLFreeHandle(SQLSMALLINT,SQLHANDLE);
foreign import #{CALLCONV} unsafe "sql.h SQLFreeHandle" sqlFreeHandle ::
  SqlSmallInt -> Handle -> IO SqlReturn

-- SQLRETURN SQL_API SQLGetDiagRec(SQLSMALLINT,SQLHANDLE,SQLSMALLINT,SQLCHAR*,SQLINTEGER*,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*); 
foreign import #{CALLCONV} unsafe "sql.h SQLGetDiagRec" sqlGetDiagRec ::
  SqlHandleType  -- ^ enum: which handle type is the next parameter?
  -> Handle  -- ^ generic handle ptr
  -> SqlSmallInt  -- ^ row (or message) number
  -> MyCString  -- ^ OUT: state
  -> Ptr SqlInteger  -- ^ OUT: error number
  -> MyCString  -- ^ OUT: error message
  -> SqlSmallInt  -- ^ IN: message buffer size
  -> Ptr SqlSmallInt  -- ^ OUT: message length
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLDriverConnect(SQLHDBC,SQLHWND,SQLCHAR*,SQLSMALLINT,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*,SQLUSMALLINT);
foreign import #{CALLCONV} unsafe "sql.h SQLDriverConnect" sqlDriverConnect ::
  ConnHandle
  -> WindowHandle  -- ^ just pass nullPtr
  -> MyCString  -- ^ connection string
  -> SqlSmallInt  -- ^ connection string size
  -> MyCString  -- ^ OUT: buffer for normalised connection string
  -> SqlSmallInt  -- ^ buffer size
  -> Ptr SqlSmallInt  -- ^ OUT: length of returned string
  -> SqlUSmallInt  -- ^ enum: should driver prompt user for missing info?
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLDisconnect(SQLHDBC);
foreign import #{CALLCONV} unsafe "sql.h SQLDisconnect" sqlDisconnect ::
  ConnHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLSetEnvAttr(SQLHENV,SQLINTEGER,SQLPOINTER,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLSetEnvAttr" sqlSetEnvAttr ::
  EnvHandle  -- ^ Env Handle
  -> SqlInteger  -- ^ Attribute (enumeration)
  -> Ptr ()  -- ^ value (cast to void*)
  -> SqlInteger  -- ^ ? - set to 0
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLSetConnectAttr(SQLHDBC,SQLINTEGER,SQLPOINTER,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLSetConnectAttr" sqlSetConnectAttr ::
  ConnHandle  -- ^ Connection Handle
  -> SqlInteger  -- ^ Attribute (enumeration)
  -> Ptr ()  -- ^ value (cast to void*)
  -> SqlInteger  -- ^ ? - set to 0
  -> IO SqlReturn

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
  -> SqlUSmallInt  -- ^ column position, 1-indexed
  -> SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> Ptr Buffer  -- ^ output buffer
  -> SqlLen  -- ^ output buffer size
  -> Ptr SqlLen -- ^ output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindCol(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindCol" sqlBindCol ::
  StmtHandle
  -> SqlUSmallInt  -- ^ column position, 1-indexed
  -> SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> Ptr Buffer  -- ^ output buffer
  -> SqlLen  -- ^ output buffer size
  -> Ptr SqlLen -- ^ output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLFetch(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLFetch" sqlFetch ::
  StmtHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindParameter(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLSMALLINT,SQLSMALLINT,SQLULEN,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindParameter" sqlBindParameter ::
  StmtHandle
  -> SqlUSmallInt  -- ^ position, 1-indexed
  -> SqlParamDirection  -- ^ direction: IN, OUT
  -> SqlCDataType  -- ^ C data type: char, int, long, float, etc
  -> SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> SqlULen  -- ^ col size (precision)
  -> SqlSmallInt  -- ^ decimal digits (scale)
  -> Ptr Buffer  -- ^ input+output buffer
  -> SqlLen  -- ^ buffer size
  -> Ptr SqlLen -- ^ input+output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLMoreResults(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLMoreResults" sqlMoreResults ::
  StmtHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLEndTran(SQLSMALLINT,SQLHANDLE,SQLSMALLINT);
foreign import #{CALLCONV} unsafe "sql.h SQLEndTran" sqlEndTran ::
  SqlSmallInt -> Handle -> SqlSmallInt -> IO SqlReturn
