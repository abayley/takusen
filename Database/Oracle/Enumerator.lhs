
> module Database.Oracle.Enumerator

Defines the concrete implemenation of the Database.Enumerator interfaces.
This module plus Database.Enumerator should be all you need to do simple database stuff.

>   (
>     -- SessionQuery is a convenient type synonym;
>     -- it makes it easier to give signatures to functions doing database stuff.
>     -- Session is the type returned by connect, and used by disconnect and runSession.
>       SessionQuery, Session
>     , connect, disconnect
>
>     -- MonadSession instance functions
>     , runSession, beginTransaction, commit, rollback, executeDML, executeDDL
>
>     -- MonadQuery instance functions
>     , doQuery, openCursor
>
>     -- Export here to avoid having to import Control.Monad.Trans.
>     -- Useful/necessary if you want to do IO in a database function.
>     , liftIO, lift
>
>     -- Useful with cursors. You should wrap cursor actions in catchreaderT;
>     -- use throwIO to rethrow exception in handler.
>     , throwIO
>
>   ) where

> import Database.Enumerator
> import Control.Monad.Trans (liftIO, lift)
> import Control.Monad.Reader (ReaderT)
> import Control.Exception (throwIO)

One or 'tother:

> import Database.Oracle.OCIEnumerator
> --import Database.Oracle.OCIStub


> type SessionQuery = ReaderT Session IO ()
