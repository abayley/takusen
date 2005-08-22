
|
Module      :  Database.MSSqlServer.MSSqlFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple wrappers for MS Sql Server functions (FFI).
 
When compiling, do not use these loader flags:
 
 > -Lc:\windows\system32 -lntwdblib
 
Instead, use:
 
 > -optl c:\windows\system32\ntwdblib.dll
 
This is because adding @c:\windows\system32@ to the linker search
path confuses the linker, as it tries to link against dlls in system32
(like msvcrt.dll), rather than the equivalent mingw dlls it normally uses.
 
Don't forget to specify the header file location
(or wherever your Sql Server installation resides):
 
 > "-IC:\Program Files\Microsoft SQL Server\80\Tools\DevTools\Include"
 

> {-# OPTIONS -ffi #-}
> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -optc-DDBNTWIN32 #-}
> {-# OPTIONS -#include "windows.h" #-}
> {-# OPTIONS -#include "sqlfront.h" #-}

Don't need to explcitly include sqldb.h, as it's implied by the
FFI foreign import declarations e.g.
  > foreign import ccall "sqldb.h dbinit" dbInit :: IO CString
implies
 {- # OPTIONS -#include "sqldb.h" #-}


> module Database.MSSqlServer.MSSqlFunctions where

> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Data.Int
> import Data.IORef



> data LoginStruct = LoginStruct
> type LoginHandle = Ptr LoginStruct
> data StmtStruct = StmtStruct
> type StmtHandle = Ptr StmtStruct
> data SessStruct = SessStruct
> type SessHandle = Ptr SessStruct
> type Blob = Ptr Word8

> type DBErrHandler = SessHandle -> CInt -> CInt -> CInt -> CString -> CString -> IO CInt
> type DBMsgHandler = SessHandle -> CInt -> CInt -> CInt -> CString -> CString -> CString -> CUShort -> IO CInt


> data MSSqlException = MSSqlException Int String
>   deriving (Typeable, Eq)

> instance Show MSSqlException where
>   show (MSSqlException i s) = "MSSqlException " ++ (show i) ++ " " ++ s

> makeEx :: CInt -> String -> MSSqlException
> makeEx e msg = MSSqlException (fromIntegral e) msg

> catchMSSql :: IO a -> (MSSqlException -> IO a) -> IO a
> catchMSSql = catchDyn

> throwMSSql :: MSSqlException -> a
> throwMSSql = throwDyn

> throwEx :: CInt -> String -> IO a
> throwEx e msg = throwMSSql (makeEx e msg)

> testForError :: CInt -> String -> a -> IO a
> testForError rc msg retval = do
>   case () of
>     _ | rc == dbSUCCEED -> return retval
>       | otherwise -> throwEx rc msg

> errorOnNull :: Ptr b -> String -> a -> IO a
> errorOnNull ptr msg retval = do
>   if ptr == nullPtr
>     then throwEx (-1 ) msg
>     else return retval

> cStr :: CStringLen -> CString
> cStr = fst
> cStrLen :: CStringLen -> CInt
> cStrLen = fromIntegral . snd

> disableErrorHandler :: IO ()
> disableErrorHandler = do
>   _ <- dbErrHandle nullFunPtr
>   return ()

> enableErrorHandler :: IORef MSSqlException -> IO ()
> enableErrorHandler exc = do
>   h <- mkErrHandler (dbErrHandler exc)
>   _ <- dbErrHandle h
>   return ()

> disableMsgHandler :: IO ()
> disableMsgHandler = do
>   _ <- dbMsgHandle nullFunPtr
>   return ()

> enableMsgHandler :: IORef MSSqlException -> IO ()
> enableMsgHandler exc = do
>   h <- mkMsgHandler (dbMsgHandler exc)
>   _ <- dbMsgHandle h
>   return ()

It doesn't seem to work if you throw an exception here.
Or rather, it does work, but the exception is not caught by the main thread.
(Must be in a separate OS thread or something.)
So, instead, we stuff the exception into an IORef, and ensure that a failure
code is returned from the original DB-Lib call.
The calling function will detect this error and raise a bland, unhelpful
MSSqlException. The exception handler catching this
can interrogate the IORef to get the real exception.

> dbErrHandler :: IORef MSSqlException -> DBErrHandler
> dbErrHandler exc sess severity dberr oserr dberrstr oserrstr = do
>   s <- peekCString dberrstr
>   putStrLn ("MS Sql Server error: " ++ s)
>   --disableErrorHandler
>   writeIORef exc (makeEx dberr s)
>   return int_CANCEL

> dbMsgHandler :: IORef MSSqlException -> DBMsgHandler
> dbMsgHandler exc sess msgno msgstate severity msgtext srvname procname line = do
>   s <- peekCString msgtext
>   putStrLn ("MS Sql Server message: " ++ s)
>   writeIORef exc (makeEx msgno s)
>   return 0



|dbinit returns CString containg DB-Lib version,
or null if init fails.

> foreign import ccall "sqldb.h dbinit" dbInit :: IO CString
> foreign import ccall "sqldb.h dbexit" dbExit :: IO ()

> foreign import ccall "wrapper" mkErrHandler ::
>   DBErrHandler -> IO (FunPtr DBErrHandler)
> foreign import ccall "wrapper" mkMsgHandler ::
>   DBMsgHandler -> IO (FunPtr DBMsgHandler)

> foreign import ccall "sqldb.h dberrhandle" dbErrHandle ::
>   FunPtr DBErrHandler -> IO (FunPtr DBErrHandler)
> foreign import ccall "sqldb.h dbmsghandle" dbMsgHandle ::
>   FunPtr DBMsgHandler -> IO (FunPtr DBMsgHandler)

> foreign import ccall "sqldb.h dblogin" dbLogin :: IO LoginHandle
> foreign import ccall "sqldb.h dbsetlname" dbSetLName ::
>   LoginHandle -> Ptr a -> CInt -> IO CInt
> foreign import ccall "sqldb.h dbopen" dbOpen ::
>   LoginHandle -> CString -> IO SessHandle
> foreign import ccall "sqldb.h dbclose" dbClose ::
>   SessHandle -> IO CInt

> foreign import ccall "sqldb.h dbcmd" dbCmd ::
>   SessHandle -> CString -> IO CInt
> foreign import ccall "sqldb.h dbsqlexec" dbSqlExec ::
>   SessHandle -> IO CInt
> foreign import ccall "sqldb.h dbresults" dbResults ::
>   SessHandle -> IO CInt
> foreign import ccall "sqldb.h dbnextrow" dbNextRow ::
>   SessHandle -> IO CInt
> foreign import ccall "sqldb.h dbdata" dbData ::
>   SessHandle -> CInt -> IO (Ptr a)
> foreign import ccall "sqldb.h dbdatlen" dbDatLen ::
>   SessHandle -> CInt -> IO CInt
> foreign import ccall "sqldb.h dbcancel" dbCancel ::
>   SessHandle -> IO CInt


> open :: String -> String -> String -> String -> IO SessHandle
> open user pswd appn srvr =
>   withCString user $ \userC ->
>   withCString pswd $ \pswdC ->
>   withCString appn $ \appnC ->
>   withCString srvr $ \srvrC -> do
>     login <- dbLogin
>     if user == ""
>       then do
>         -- Use Windows-authenticated logon
>         dbSetLName login nullPtr dbSETSECURE
>       else do
>         -- Normal username+password logon
>         dbSetLName login (castPtr userC) dbSETUSER
>         dbSetLName login (castPtr pswdC) dbSETPWD
>     dbSetLName login appnC dbSETAPP
>     sess <- dbOpen login srvrC
>     errorOnNull sess "dbOpen" sess


> close :: SessHandle -> IO ()
> close sess = do
>   rc <- dbClose sess
>   testForError rc "dbClose" ()

> execQuery :: SessHandle -> String -> IO ()
> execQuery sess sql = do
>   withCString sql $ \sqlC -> do
>     rc <- dbCmd sess sqlC
>     testForError rc "dbCmd" ()
>     rc <- dbSqlExec sess
>     testForError rc "dbSqlExec" ()
>     rc <- dbResults sess
>     testForError rc "dbResults" ()

> fetchRow :: SessHandle -> IO CInt
> fetchRow sess = dbNextRow sess


> colValString :: SessHandle -> Int -> IO String
> colValString sess col = do
>   ps <- dbData sess (fromIntegral col)
>   l <- dbDatLen sess (fromIntegral col)
>   peekCStringLen (ps, fromIntegral l)

> colValAny :: (Storable a) => SessHandle -> Int -> IO a
> colValAny sess col = do
>   p <- dbData sess (fromIntegral col)
>   peek p

> colValCInt :: SessHandle -> Int -> IO CInt
> colValCInt = colValAny

> colValInt :: SessHandle -> Int -> IO Int
> colValInt sess col = do
>   i <- colValCInt sess col
>   return (fromIntegral i)

> colValCDouble :: SessHandle -> Int -> IO CDouble
> colValCDouble = colValAny

> colValDouble :: SessHandle -> Int -> IO Double
> colValDouble sess col = do
>   d <- colValCDouble sess col
>   return (realToFrac d)


|Macros for dbsetlname()

> dbSETHOST
>   , dbSETUSER
>   , dbSETPWD
>   , dbSETAPP
>   , dbSETID
>   , dbSETLANG
>   , dbSETSECURE
>   , dbVER42
>   , dbVER60
>   , dbSETLOGINTIME
>   , dbSETFALLBACK
>   :: CInt
> dbSETHOST = 1
> dbSETUSER = 2
> dbSETPWD = 3
> dbSETAPP = 4
> dbSETID = 5
> dbSETLANG = 6
> dbSETSECURE = 7
> dbVER42 = 8
> dbVER60 = 9
> dbSETLOGINTIME = 10
> dbSETFALLBACK = 12

|Various constants and return codes.
Culled from C:\Program Files\Microsoft SQL Server\80\Tools\DevTools\Include\sqlfront.h

> dbSUCCEED
>   , dbFAIL
>   , dbSUCCEEDABORT
>   , dbNO_MORE_RESULTS
>   , dbNO_MORE_RPC_RESULTS
>   , dbMORE_ROWS
>   , dbNO_MORE_ROWS
>   , dbREG_ROW
>   , dbBUF_FULL
>   , int_EXIT
>   , int_CONTINUE
>   , int_CANCEL
>   :: CInt
> dbSUCCEED = 1
> dbFAIL = 0
> dbSUCCEEDABORT = 2
> dbNO_MORE_RESULTS = 2
> dbNO_MORE_RPC_RESULTS = 3
> dbMORE_ROWS = -1
> dbNO_MORE_ROWS = -2
> dbREG_ROW = -2
> dbBUF_FULL = -3

> int_EXIT = 0
> int_CONTINUE = 1
> int_CANCEL = 2
