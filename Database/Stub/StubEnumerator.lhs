
|
Module      :  Database.Stub.StubEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Stub implementation of Database.Enumerator.
Useful for people who can't or won't install a DBMS,
so that they can try out the Enumerator interface.
 
Currently last last row of any fetch will have a null in its Int columns
(this makes it easier to test handling of nulls and DBUnexpectedNull).
See fetchIntVal.



> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Stub.StubEnumerator where


> import Database.Enumerator
> import Foreign
> import Foreign.C
> import Foreign.C.Types
> import Control.Monad
> import Control.Monad.State
> import Control.Exception (catchDyn, throwDyn, throwIO)
> import System.Time
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.IORef
> import Data.Dynamic



--------------------------------------------------------------------
-- Stubs for OCI function wrappers.
--------------------------------------------------------------------


> data Session = Session 

> data Query = Query
>   { stmtHandle :: ()
>   , fetchCounter :: IORef (IORef Int)
>   }

> connect :: String -> String -> String -> IO Session
> connect user pswd dbname = return Session

> disconnect :: Session -> IO ()
> disconnect session = return ()


--------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------


> instance MonadSession (ReaderT Session IO) IO Session where
>   runSession = flip runReaderT
>   getSession = ask
>   beginTransaction isolation = return ()
>   commit = return ()
>   rollback = return ()
>   executeDML cmdText = return 0
>   executeDDL cmdText = return ()



--------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------

See makeQuery below for use of this:

> numberOfRowsToPretendToFetch :: Int
> numberOfRowsToPretendToFetch = 3

See fetchIntVal below for use of this:
Note that rows are counted down from numberOfRowsToPretendToFetch,
so this will throw on the last row.

> throwNullIntOnRow :: Int
> throwNullIntOnRow = 1



> type StubMonadQuery = ReaderT Query (ReaderT Session IO)

> instance MonadQuery StubMonadQuery (ReaderT Session IO) Query ColumnBuffer
>  where
>
>   runQuery m query = runReaderT m query
>
>   getQuery = ask
>
>   makeQuery sqltext resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ return ()
>     -- Leave one counter in to ensure the fetch terminates
>     counter <- liftIO $ newIORef numberOfRowsToPretendToFetch >>= newIORef
>     return $ Query stmt counter
>
>   fetch1Row = do
>     query <- getQuery
>     -- We'll pretend that we're going to fetch a finite number of rows.
>     refCounter <- liftIO $ readIORef (fetchCounter query)
>     counter <- liftIO $ readIORef refCounter
>     if counter > 0
>       then (liftIO $ writeIORef refCounter (counter - 1) >> return True)
>       else return False
>
>   allocBuffer (bufsize, buftype) colpos = do
>     query <- getQuery
>     return $ ColumnBuffer
>       { colPos = colpos
>       }
>
>   columnPosition buffer = return (colPos buffer)
>
>   currentRowNum = do
>     query <- getQuery
>     refCounter <- liftIO $ readIORef (fetchCounter query)
>     counter <- liftIO $ readIORef refCounter
>     return counter
>
>   freeBuffer buffer = return ()
>
>   -- after buffers are freed, close the STMT
>   destroyQuery query = return ()



--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   }


> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer = return $ Just "boo"

> bufferToDatetime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToDatetime colbuf = do
>       return $ Just $ CalendarTime
>         { ctYear = 1971
>         , ctMonth = toEnum 6
>         , ctDay = 1
>         , ctHour = 12
>         , ctMin = 1
>         , ctSec = 1
>         , ctPicosec = 0
>         , ctWDay = Sunday
>         , ctYDay = -1
>         , ctTZName = "UTC"
>         , ctTZ = 0
>         , ctIsDST = False
>         }

> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt buffer = return $ Just 1

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble buffer = return $ Just 1.1



> instance DBType (Maybe a) StubMonadQuery ColumnBuffer
>     => DBType a StubMonadQuery ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol buffer = throwIfDBNull buffer fetchCol
>   bind pos val = return ()

> instance DBType (Maybe String) StubMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (4000, DBTypeString) n
>   fetchCol buffer = liftIO $ bufferToString buffer
>   bind pos val = return ()

> instance DBType (Maybe Int) StubMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (4, DBTypeInt) n
>   fetchCol buffer = do
>     query <- getQuery
>     refCounter <- liftIO $ readIORef (fetchCounter query)
>     counter <- liftIO $ readIORef refCounter
>     -- last row returns null rather than 1
>     if counter == throwNullIntOnRow
>       then return Nothing
>       else liftIO $ bufferToInt buffer
>   bind pos val = return ()

> instance DBType (Maybe Double) StubMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = liftIO $ bufferToDouble buffer
>   bind pos val = return ()

> instance DBType (Maybe CalendarTime) StubMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDatetime) n
>   fetchCol buffer = liftIO $ bufferToDatetime buffer
>   bind pos val = return ()


A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) StubMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = do
>     v <- liftIO$ bufferToString buffer
>     case v of
>       Just s -> return (Just (read s))
>       Nothing -> return Nothing
>   bind pos val = return ()
