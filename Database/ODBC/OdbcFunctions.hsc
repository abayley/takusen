
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
 
type SqlInteger = #{type SQLINTEGER}
type SqlUInteger = #{type SQLUINTEGER}
type SqlSmallInt = #{type SQLSMALLINT}
type SqlUSmallInt = #{type SQLUSMALLINT}
type SqlLen = #{type SQLLEN}
type SqlReturn = SqlSmallInt
type SqlHandleType = SqlSmallInt
type SqlDataType = SqlSmallInt

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

-- ODBC data types
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
  #{const SQL_TYPE_DATE} :
  #{const SQL_TYPE_TIME} :
  #{const SQL_TYPE_TIMESTAMP} :
  []) :: [SqlDataType]


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

getDiagRec :: SqlReturn -> SqlHandleType -> Handle -> SqlSmallInt -> IO [OdbcException]
getDiagRec retcode htype handle row =
  allocaBytes 6 $ \cstrState -> do
  alloca $ \errorNumPtr -> do
  allocaBytes 1025 $ \cstrMsg -> do
  alloca $ \msgLenPtr -> do
    rc <- sqlGetDiagRec htype handle row cstrState errorNumPtr cstrMsg 1024 msgLenPtr
    if rc == sqlRcNoData
      then return []
      else do
      errnum <- peek errorNumPtr
      state <- myPeekCStringLen (cstrState, 5)
      msglen <- peek msgLenPtr
      putStrLn ("getDiagRec: msglen=" ++ show msglen)
      msg <- myPeekCStringLen (cstrMsg, fromIntegral msglen)
      putStrLn ("getDiagRec: msg=" ++ msg)
      more <- getDiagRec retcode htype handle (row+1)
      return ((OdbcException (fromIntegral errnum) state msg []) : more)

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
    checkError rc sqlHTypeEnv h
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

bindColBuffer :: StmtHandle -> Int -> SqlDataType -> Int -> IO BindBuffer
bindColBuffer stmt pos dtype size = do
  bfptr <- mallocForeignPtrBytes size
  szfptr <- mallocForeignPtr
  withForeignPtr bfptr $ \bptr -> do
  withForeignPtr szfptr $ \szptr -> do
  rc <- sqlBindCol stmt (fromIntegral pos) dtype bptr (fromIntegral size) szptr
  checkError rc sqlHTypeStmt (castPtr stmt)
  return (BindBuffer (bfptr, szfptr))

-- | Return 'True' if there are more rows, 'False' if end-of-data.
fetch :: StmtHandle -> IO Bool
fetch stmt = do
  rc <- sqlFetch stmt
  when (rc /= sqlRcNoData)
    (checkError rc sqlHTypeStmt (castPtr stmt))
  return (rc /= sqlRcNoData)


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

getAsciiStringFromBuffer :: BindBuffer -> IO (Maybe String)
getAsciiStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekCAStringLen (castPtr ptr, fromIntegral len))

getCWStringFromBuffer :: BindBuffer -> IO (Maybe String)
getCWStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekCWStringLen (castPtr ptr, fromIntegral len))

getUTF8StringFromBuffer :: BindBuffer -> IO (Maybe String)
getUTF8StringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekUTF8StringLen (castPtr ptr, fromIntegral len))

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

getUtcTimeFromBuffer :: BindBuffer -> IO (Maybe UTCTime)
getUtcTimeFromBuffer bindbuffer = do
  testForNull bindbuffer $ \buffer _ -> do
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


foreign import #{CALLCONV} unsafe "takusen_cbits.h getSqlOvOdbc3" getSqlOvOdbc3 ::
  Ptr ()

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

-- SQLRETURN SQL_API SQLPrepare(SQLHSTMT,SQLCHAR*,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLPrepare" sqlPrepare ::
  StmtHandle -> MyCString -> SqlInteger -> IO SqlReturn

-- SQLRETURN SQL_API SQLExecute(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLExecute" sqlExecute ::
  StmtHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLDriverConnect(SQLHDBC,SQLHWND,SQLCHAR*,SQLSMALLINT,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*,SQLUSMALLINT);
foreign import #{CALLCONV} unsafe "sql.h SQLDriverConnect" sqlDriverConnect ::
  ConnHandle -> WindowHandle -> MyCString -> SqlSmallInt ->
  MyCString -> SqlSmallInt -> Ptr SqlSmallInt -> SqlUSmallInt -> IO SqlReturn

-- SQLRETURN SQL_API SQLDisconnect(SQLHDBC);
foreign import #{CALLCONV} unsafe "sql.h SQLDisconnect" sqlDisconnect ::
  ConnHandle -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindCol(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindCol" sqlBindCol ::
  StmtHandle
  -> SqlUSmallInt  -- column position, 1-indexed
  -> SqlDataType  -- data type: string, int, long, date, etc
  -> Ptr Buffer  -- output buffer
  -> SqlLen  -- output buffer size
  -> Ptr SqlLen -- output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLFetch(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLFetch" sqlFetch ::
  StmtHandle -> IO SqlReturn
