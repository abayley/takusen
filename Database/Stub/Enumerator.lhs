
|
Module      :  Database.Stub.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Stub implemenation of the "Database.Enumerator" interfaces.
"Database.Stub.Enumerator" should be all you need to import
to try the Enumerator interface;
the useful stuff from "Database.Enumerator" is re-exported.
 

> module Database.Stub.Enumerator
>   (
>     -- *Re-export Database.Enumerator
>       DBColumnType(..), IsolationLevel(..), IterAct, DBException(..)
>     , catchDB, throwDB, basicDBExceptionReporter, catchDBError, ignoreDBError
>     , shakeReaderT, catchReaderT
>     , cursorIsEOF, cursorCurrent, cursorNext, cursorClose
>     , withCursorBracket, withCursorBracketTuned
>     , withTransaction
>     , ifNull, result, result'
>
>     -- **'MonadSession' instance functions
>     , runSess, runSession, beginTransaction, commit, rollback, executeDML, executeDDL
>
>     -- **'MonadQuery' instance functions
>     , DBCursor, MonadQuery, QueryResourceUsage(..)
>     , doQuery, doQueryTuned
>
>     -- *Sessions: connecting etc
>     -- |'SessionQuery' is a convenient type synonym;
>     -- it makes it easier to give signatures to functions doing database stuff.
>     -- 'Session' is the type returned by connect, and used by disconnect and runSession.
>     , SessionQuery, Session
>     , connect, disconnect
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


> import Database.Stub.StubEnumerator


> type SessionQuery = ReaderT Session IO ()
