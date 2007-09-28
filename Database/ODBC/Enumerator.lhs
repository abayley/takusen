
|
Module      :  Database.ODBC.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
ODBC implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.ODBC.Enumerator
>   ( Session, connect
>   , prepareStmt, preparePrefetch
>   , prepareQuery, prepareLargeQuery, prepareCommand
>   , sql, sqlbind, prefetch, cmdbind
>   , module Database.Enumerator
>   )
> where


> import Database.Enumerator
> import Database.InternalEnumerator
> import Database.Util
> import Foreign.C
> import Foreign.Storable (sizeOf)
> import Control.Monad
> import Control.Exception
> import Database.ODBC.OdbcFunctions
>   (EnvHandle, ConnHandle, StmtHandle, OdbcException(..), catchOdbc, throwOdbc)
> import qualified Database.ODBC.OdbcFunctions as DBAPI
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.Dynamic
> import Data.IORef
> import Data.Int
> import System.Time
> import Data.Time



--------------------------------------------------------------------
-- ** API Wrappers
--------------------------------------------------------------------

|These wrappers ensure that only DBExceptions are thrown,
and never SqliteExceptions.
We don't need wrappers for the colValXxx functions
because they never throw exceptions.


> convertAndRethrow :: OdbcException -> IO a
> convertAndRethrow (OdbcException e st m exs) = do
>   let
>     statepair@(stateclass, statesubclass) = (take 2 st, drop 2 st)
>     ec = case stateclass of
>       "XX" -> DBFatal
>       "58" -> DBFatal
>       "57" -> DBFatal
>       "54" -> DBFatal
>       "53" -> DBFatal
>       "08" -> DBFatal
>       _ -> DBError
>   throwDB (ec statepair e m)


|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = catchOdbc action convertAndRethrow

> stmtPrepare :: ConnHandle -> String -> IO StmtHandle
> stmtPrepare conn sqltext = convertEx $ do
>   stmt <- DBAPI.allocStmt conn
>   DBAPI.prepareStmt stmt sqltext
>   return stmt

> stmtExecute :: StmtHandle -> IO ()
> stmtExecute stmt = convertEx (DBAPI.executeStmt stmt)

> closeCursor :: StmtHandle -> IO ()
> closeCursor stmt = convertEx (DBAPI.closeCursor stmt)

> fetchRow :: StmtHandle -> IO Bool
> fetchRow stmt = convertEx (DBAPI.fetch stmt)

> rowCount :: StmtHandle -> IO Int
> rowCount stmt = convertEx (DBAPI.rowCount stmt)

> freeStmt stmt = convertEx (DBAPI.freeStmt stmt)
> freeConn conn = convertEx (DBAPI.freeConn conn)
> freeEnv env = convertEx (DBAPI.freeEnv env)

> connectDb connstr = convertEx $ do
>   env <- DBAPI.allocEnv
>   DBAPI.setOdbcVer env
>   conn <- DBAPI.allocConn env
>   connstr <- DBAPI.connect conn connstr
>   return (env, conn)

> disconnectDb conn = convertEx (DBAPI.disconnect conn)

> commitTrans conn = convertEx (DBAPI.commit conn)
> rollbackTrans conn = convertEx (DBAPI.rollback conn)

--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

We don't need much in a Session record.
Session objects are created by 'connect'.

> data Session = Session
>   { envHandle :: EnvHandle
>   , connHandle :: ConnHandle }
>   deriving Typeable

> connect :: String -> ConnectA Session
> connect connstr = ConnectA $ do
>   (env, conn) <- connectDb connstr
>   return (Session env conn)

--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------

> newtype QueryString = QueryString String

> sql :: String -> QueryString
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand sess (QueryString str) = executeCommand sess str

> instance Command String Session where
>   executeCommand sess str = do
>     stmt <- stmtPrepare (connHandle sess) str
>     stmtExecute stmt
>     n <- rowCount stmt
>     freeStmt stmt
>     return (fromIntegral n)

> instance Command BoundStmt Session where
>   executeCommand sess (BoundStmt pstmt) =
>     rowCount (stmtHandle pstmt)

> instance Command StmtBind Session where
>   executeCommand sess (StmtBind sqltext bas) = do
>     let (PreparationA action) = prepareStmt' sqltext FreeManually
>     pstmt <- action sess
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     let stmt = stmtHandle pstmt
>     stmtExecute stmt
>     n <- rowCount stmt
>     freeStmt stmt
>     return n

> instance ISession Session where
>   disconnect sess = do
>     disconnectDb (connHandle sess)
>     freeConn (connHandle sess)
>     freeEnv (envHandle sess)
>   -- With ODBC, transactions a implicitly started.
>   -- There is no beginTrans.
>   beginTransaction sess isolation = return ()
>   commit sess = commitTrans (connHandle sess)
>   rollback sess = rollbackTrans (connHandle sess)

About stmtFreeWithQuery:

We need to keep track of the scope of the PreparedStmtObj
i.e. should it be freed when the Query (result-set) is freed,
or does it have a longer lifetime?
PreparedStmts created by prepareStmt have a lifetime possibly
longer than the result-set; users should use withPreparedStatement
to manage these.

PreparedStmts can also be created internally by various instances
of makeQuery (in class Statement), and these will usually have the
same lifetime/scope as that of the Query (result-set).

This lifetime distinction should probably be handled by having
separate types for the two types of prepared statement...

> data StmtLifetime = FreeWithQuery | FreeManually

> data PreparedStmtObj = PreparedStmtObj
>   { stmtHandle :: StmtHandle
>   , stmtLifetime :: StmtLifetime
>   }

> prepareStmt :: QueryString -> PreparationA Session PreparedStmtObj
> prepareStmt (QueryString sqltext) = prepareStmt' sqltext FreeManually

> prepareQuery :: QueryString -> PreparationA Session PreparedStmtObj
> prepareQuery (QueryString sqltext) = prepareStmt' sqltext FreeManually

> prepareLargeQuery :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> prepareLargeQuery _ (QueryString sqltext) = prepareStmt' sqltext FreeManually

> prepareCommand :: QueryString -> PreparationA Session PreparedStmtObj
> prepareCommand (QueryString sqltext) = prepareStmt' sqltext FreeManually


preparePrefetch is just here for interface consistency
with Oracle and PostgreSQL.

> preparePrefetch :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> preparePrefetch count (QueryString sqltext) =
>   prepareStmt' sqltext FreeManually

> prepareStmt' sqltext free =
>   PreparationA (\sess -> do
>     stmt <- stmtPrepare (connHandle sess) sqltext
>     return (PreparedStmtObj stmt free))

--------------------------------------------------------------------
-- ** Binding
--------------------------------------------------------------------

> newtype BoundStmt = BoundStmt { boundStmt :: PreparedStmtObj }
> type BindObj = Int -> IO ()

> instance IPrepared PreparedStmtObj Session BoundStmt BindObj where
>   bindRun sess pstmt bas action = do
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     let stmt = stmtHandle pstmt
>     stmtExecute stmt
>     action (BoundStmt pstmt)
>   destroyStmt sess pstmt = freeStmt (stmtHandle pstmt)

> instance DBBind (Maybe String) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer

> instance DBBind (Maybe Int) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer

> instance DBBind (Maybe Double) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer

> instance DBBind (Maybe UTCTime) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer

> instance DBBind (Maybe a) Session PreparedStmtObj BindObj
>     => DBBind a Session PreparedStmtObj BindObj where
>   bindP x = bindP (Just x)

The default instance, uses generic Show

> instance (Show a) => DBBind (Maybe a) Session PreparedStmtObj BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")

> makeBindAction val binder = BindA (\ses st pos -> do
>   convertEx (binder (stmtHandle st) pos val >> return ()))

--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data Query = Query
>   { queryStmt :: PreparedStmtObj
>   , querySess :: Session
>   , queryCount :: IORef Int
>   }

> data StmtBind = StmtBind String [BindA Session PreparedStmtObj BindObj]

> sqlbind :: String -> [BindA Session PreparedStmtObj BindObj] -> StmtBind
> sqlbind sql bas = StmtBind sql bas

> cmdbind :: String -> [BindA Session PreparedStmtObj BindObj] -> StmtBind
> cmdbind sql bas = StmtBind sql bas

> prefetch :: Int -> String -> [BindA Session PreparedStmtObj BindObj] -> StmtBind
> prefetch n sql bas = StmtBind sql bas


> instance Statement BoundStmt Session Query where
>   makeQuery sess bstmt = do
>     n <- newIORef 0
>     return (Query (boundStmt bstmt) sess n)

> instance Statement PreparedStmtObj Session Query where
>   makeQuery sess pstmt = do
>     stmtExecute (stmtHandle pstmt)
>     n <- newIORef 0
>     return (Query pstmt sess n)

> instance Statement StmtBind Session Query where
>   makeQuery sess (StmtBind sqltext bas) = do
>     let (PreparationA action) = prepareStmt' sqltext FreeWithQuery
>     pstmt <- action sess
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     stmtExecute (stmtHandle pstmt)
>     n <- newIORef 0
>     return (Query pstmt sess n)

> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = makeQuery sess sqltext

> instance Statement String Session Query where
>   makeQuery sess sqltext = do
>     let (PreparationA action) = prepareStmt' sqltext FreeWithQuery
>     pstmt <- action sess
>     stmtExecute (stmtHandle pstmt)
>     n <- newIORef 0
>     return (Query pstmt sess n)


> instance IQuery Query Session ColumnBuffer where
>   destroyQuery query = do
>     closeCursor (stmtHandle (queryStmt query))
>     case stmtLifetime (queryStmt query) of
>       FreeWithQuery -> freeStmt (stmtHandle (queryStmt query))
>       FreeManually -> return ()
>   fetchOneRow query = do
>     moreRows <- fetchRow (stmtHandle (queryStmt query))
>     modifyIORef (queryCount query) (+1)
>     return moreRows
>   currentRowNum q = readIORef (queryCount q)
>   freeBuffer q buffer = return ()


> nullIf :: Bool -> a -> Maybe a
> nullIf test v = if test then Nothing else Just v

> --bufferToString buffer =
> --  convertEx (DBAPI.getUTF8StringFromBuffer (colBuffer buffer))


> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   , colBuffer :: DBAPI.BindBuffer
>   }

> allocBuffer q pos size val = do
>   --putStrLn ("allocBuffer: pos " ++ show pos ++ ", size " ++ show size)
>   bindbuffer <- convertEx (DBAPI.bindColBuffer (stmtHandle (queryStmt q)) pos size val)
>   return (ColumnBuffer pos bindbuffer)

> buffer_pos q buffer = do
>   row <- currentRowNum q
>   return (row,colPos buffer)


> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 32000 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 0 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 0 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))

> instance DBType (Maybe UTCTime) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 32 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))


|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor v q n = allocBufferFor (Just v) q n
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) (fetchCol q buffer)


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 32000 (Just "")
>   fetchCol q buffer = do
>     v <- convertEx (DBAPI.getFromBuffer (colBuffer buffer))
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing
