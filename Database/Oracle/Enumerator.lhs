
|
Module      :  Database.Oracle.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Defines the concrete implemenation of the "Database.Enumerator" interfaces.
"Database.Oracle.Enumerator" should be all you need to import
in order to do simple Oracle database stuff
(the useful stuff from "Database.Enumerator" is re-exported).
 
If you want to use the real Oracle OCI implementation
then uncomment OCIEnumberator and comment OCIStub.
If you want to use the stub
then comment OCIEnumberator and uncomment OCIStub.


> module Database.Oracle.Enumerator
>   (
>     -- *Re-export Database.Enumerator
>       DBColumnType(..), IsolationLevel(..), IterAct, DBException(..)
>     , catchDB, throwDB, basicDBExceptionReporter, catchDBError, ignoreDBError
>     , shakeReaderT, catchReaderT
>     , cursorIsEOF, cursorCurrent, cursorNext, cursorClose, withCursorBracket
>     , ifNull
>
>     -- *Sessions: connecting etc
>     -- |'SessionQuery' is a convenient type synonym;
>     -- it makes it easier to give signatures to functions doing database stuff.
>     -- 'Session' is the type returned by connect, and used by disconnect and runSession.
>     , SessionQuery, Session
>     , connect, disconnect
>
>     -- *'MonadSession' instance functions
>     , runSession, beginTransaction, commit, rollback, executeDML, executeDDL
>
>     -- *'MonadQuery' instance functions
>     , doQuery, openCursor
>
>     -- *Utility functions
>     -- |Export here to avoid having to import Control.Monad.Trans.
>     -- Useful if you want to do IO in a database function.
>     , liftIO, lift
>
>     -- |Useful with cursors. You should wrap cursor actions in catchreaderT;
>     -- use throwIO to rethrow exception in handler.
>     , throwIO
>
>   ) where

> import Database.Enumerator
> import Control.Monad.Trans (liftIO, lift)
> import Control.Monad.Reader (ReaderT)
> import Control.Exception (throwIO)


One or t'other:

> import Database.Oracle.OCIEnumerator
> --import Database.Oracle.OCIStub


> type SessionQuery = ReaderT Session IO ()
