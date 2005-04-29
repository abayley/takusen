
|
Module      :  Database.MSSqlServer.MSSqlFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple wrappers for MS Sql Server functions (FFI).
 
When compiling, do not use loader flags:
  @-Lc:\windows\system32 -lntwdblib@
Instead, use:
  @-optl c:\windows\system32\ntwdblib.dll@
This is because adding @c:\windows\system32@ as a linker search
path confuses it, as it tries to link against dlls in system32
(like @msvcrt.dll@), rather than the equivalent mingw dlls it normally uses


> {-# OPTIONS -ffi #-}
> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -optc-DDBNTWIN32 #-}
> {-# OPTIONS -#include "windows.h" #-}
> {-# OPTIONS -#include "sqlfront.h" #-}

Don't need this, as it's implied by the FFI foreign imports.
{- # OPTIONS -#include "sqldb.h" #-}


> module Database.MSSqlServer.MSSqlFunctions where

> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Data.Int



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
>   deriving (Typeable)

> instance Show MSSqlException where
>   show (MSSqlException i s) = "MSSqlException " ++ (show i) ++ " " ++ s

> catchMSSql :: IO a -> (MSSqlException -> IO a) -> IO a
> catchMSSql = catchDyn

> throwMSSql :: MSSqlException -> a
> throwMSSql = throwDyn

> throwEx :: CInt -> String -> a
> throwEx e msg = throwMSSql (MSSqlException (fromIntegral e) msg)
> --throwEx e msg = error msg

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

> enableErrorHandler :: IO ()
> enableErrorHandler = do
>   h <- mkErrHandler dbErrHandler
>   _ <- dbErrHandle h
>   return ()

> disableMsgHandler :: IO ()
> disableMsgHandler = do
>   _ <- dbMsgHandle nullFunPtr
>   return ()

> enableMsgHandler :: IO ()
> enableMsgHandler = do
>   h <- mkMsgHandler dbMsgHandler
>   _ <- dbMsgHandle h
>   return ()

> dbErrHandler :: DBErrHandler
> dbErrHandler sess severity dberr oserr dberrstr oserrstr = do
>   s <- peekCString dberrstr
>   putStrLn ("MS Sql Server error: " ++ s)
>   disableErrorHandler
>   putStrLn "error handler disabled"
>   throwEx dberr s

> dbMsgHandler :: DBMsgHandler
> dbMsgHandler sess msgno msgstate severity msgtext srvname procname line = do
>   s <- peekCString msgtext
>   putStrLn ("MS Sql Server message: " ++ s)
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

> dbSUCCEED
>   , dbFAIL
>   , dbSUCCEEDABORT
>   :: CInt
> dbSUCCEED = 1
> dbFAIL = 0
> dbSUCCEEDABORT = 2
