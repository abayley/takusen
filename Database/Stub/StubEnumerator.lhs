
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

> module Database.Stub.StubEnumerator
>   -- Session is not exported, not even its type!
>   ( Session, ConnParm(..), connect, sql )
> where


> import Database.InternalEnumerator
> import Foreign
> import Foreign.C
> import Foreign.C.Types
> import Control.Monad
> import Control.Exception (catchDyn, throwDyn, throwIO)
> import System.Time
> import Data.IORef
> import Data.Dynamic



> data ConnParm = ConnParm{ user, pswd, dbname :: String }


> data Session = Session
> data StmtHandle = StmtHandle
> data QueryString = QueryString String

 data PreparedStatement = PreparedStatement
   { stmtSession :: Session, stmtHandle :: StmtHandle }

> data Query = Query
>   { querySess :: Session
>   , queryStmt :: StmtHandle
>   , queryCounter :: IORef (IORef Int)
>   }

> data DBColumnType =
>     DBTypeInt
>   | DBTypeString
>   | DBTypeDouble
>   | DBTypeDatetime

> type BufferSize = Int


--------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------

> connect connparm = return Session

> instance ISession Session where
>   disconnect sess = return ()
>   beginTransaction sess isol  = return ()
>   commit sess = return ()
>   rollback sess = return ()

> {-
> instance MonadSession SessionM IO Session PreparedStatement where
>   runSession = flip runReaderT
>   getSession = ask
>   beginTransaction isolation = return ()
>   commit = return ()
>   rollback = return ()
>   executeDML cmdText args = return 0
>   executeDDL cmdText args = return ()
>   withStatement sqltext resourceUsage action = do
>     sess <- getSession
>     let s = PreparedStatement sess StmtHandle
>     action s
>   executeStatement stmt = return 0
>   prepareStatement sqltext resourceUsage = do
>     sess <- getSession
>     return (PreparedStatement sess StmtHandle)
>   freeStatement stmt = return ()
>   bindParameters stmt acts = return ()
> -}

--------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------

-- Simple statements: just a string

> sql str = QueryString str

> instance Statement QueryString Session Query where
>   executeDML s q  = return 0
>   executeDDL s q  = return ()
>   makeQuery sess stmt = do
>       -- Leave one counter in to ensure the fetch terminates
>     counter <- newIORef numberOfRowsToPretendToFetch
>     refc <- newIORef counter
>     return (Query sess StmtHandle refc)


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


> instance IQuery Query Session
>   where
>
>   fetchOneRow q = do
>     -- We'll pretend that we're going to fetch a finite number of rows.
>     refCounter <- readIORef (queryCounter q)
>     counter <- readIORef refCounter
>     if counter > 0
>       then (modifyIORef refCounter pred >> return True)
>       else return False
>
>
>   currentRowNum q = do
>     refCounter <- readIORef (queryCounter q)
>     counter <- readIORef refCounter
>     return counter
>
>   -- freeBuffer buffer = return ()
>


> {-
> type QueryM = ReaderT Query SessionM

> instance MonadQuery QueryM SessionM PreparedStatement ColumnBuffer Query
>  where
>
>   runQuery = flip runReaderT
>
>   getQuery = ask
>
>   makeQuery stmt bindacts = do
>     -- Leave one counter in to ensure the fetch terminates
>     counter <- liftIO $ newIORef numberOfRowsToPretendToFetch
>     refc <- liftIO $ newIORef counter
>     return (Query stmt refc)
>
>   fetchOneRow = do
>     query <- getQuery
>     -- We'll pretend that we're going to fetch a finite number of rows.
>     refCounter <- liftIO $ readIORef (queryCounter query)
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
>     refCounter <- liftIO $ readIORef (queryCounter query)
>     counter <- liftIO $ readIORef refCounter
>     return counter
>
>   freeBuffer buffer = return ()
> -}


--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   }

> buffer_pos q buffer = 
>     do 
>     let col = colPos buffer
>     row <- currentRowNum q
>     return (row,col)

An auxiliary function: buffer allocation

> allocBuffer q bufsize buftype colpos = do
>     return $ ColumnBuffer
>       { colPos = colpos
>       }

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

> {-
> instance DBBind (Maybe a) SessionM PreparedStatement
>     => DBBind a SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe String) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe Int) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe Double) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe CalendarTime) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance (Show a, Read a) => DBBind (Maybe a) SessionM PreparedStatement where
>   bindPos v q p = return ()
> -}

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) $ fetchCol q buffer
>   freeBuffer q = return ()

> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q 4000 DBTypeString n
>   fetchCol q buffer = bufferToString buffer

> {-
> instance DBType (Maybe Int) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (4, DBTypeInt) n
>   fetchCol buffer = do
>     query <- getQuery
>     refCounter <- liftIO $ readIORef (queryCounter query)
>     counter <- liftIO $ readIORef refCounter
>     -- last row returns null rather than 1
>     if counter == throwNullIntOnRow
>       then return Nothing
>       else liftIO $ bufferToInt buffer

> instance DBType (Maybe Double) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDouble) n
>   fetchCol buffer = liftIO $ bufferToDouble buffer

> instance DBType (Maybe CalendarTime) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer (8, DBTypeDatetime) n
>   fetchCol buffer = liftIO $ bufferToDatetime buffer
> -}

A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _  = allocBufferFor (undefined::String)
>   fetchCol q buffer = do
>     v <- bufferToString buffer
>     case v of
>       Just s -> return (Just (read s))
>       Nothing -> return Nothing

