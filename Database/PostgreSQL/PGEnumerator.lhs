
|
Module      :  Database.PostgreSQL.PGEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
PostgreSQL implementation of Database.Enumerator.


Can we support database functions that return/create multiple result-sets?
Oracle, MS Sql Server, and Sybase have them,
and we can simulate them in Postgres with the code below.

CREATE TRUSTED PROCEDURAL LANGUAGE 'plpgsql'
  HANDLER plpgsql_call_handler;

DROP FUNCTION myfunc() ;

CREATE FUNCTION myfunc() RETURNS SETOF refcursor AS $$
DECLARE
    refc1 refcursor;
    refc2 refcursor;
BEGIN

    OPEN refc1 FOR SELECT * FROM pg_database;
    RETURN NEXT refc1;
    OPEN refc2 FOR SELECT * FROM pg_proc;
    RETURN NEXT refc2;
END;
$$ LANGUAGE plpgsql;

-- this select returns two values (rows), both strings (well, refcursors),
-- which are the cursor names.
select * from myfunc();


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.PostgreSQL.PGEnumerator
>   -- Only the type constructor of Session is exported
>   -- (so the end user could write type signatures). 
>   ( Session, connect, ConnectAttr(..)
>    , sql
>    , QueryResourceUsage(..), prefetch
>    , PreparedStmt, prepareStmt, bindType
>    , module Database.Enumerator
>   )
> where


> import qualified Database.Enumerator
> import Database.Enumerator (print_)
> import Database.InternalEnumerator
> import Foreign.C
> import Control.Monad
> import Control.Exception (catchDyn, throwDyn, throwIO)
> import Database.PostgreSQL.PGFunctions
>   (DBHandle, ResultSetHandle, PGException(..), catchPG, throwPG)
> import qualified Database.PostgreSQL.PGFunctions as DBAPI
> import Data.IORef
> import Data.Int
> import System.Time


--------------------------------------------------------------------
-- ** API Wrappers
--------------------------------------------------------------------

|These wrappers ensure that only DBExceptions are thrown,
and never PGExceptions.
We don't need wrappers for the colValXxx functions
because they never throw exceptions.


> convertAndRethrow :: PGException -> IO a
> convertAndRethrow (PGException e m) = do
>   let
>     s@(ssc,sssc) = errorSqlState e
>     ec = case ssc of
>       "XX" -> DBFatal
>       "58" -> DBFatal
>       "57" -> DBFatal
>       "54" -> DBFatal
>       "53" -> DBFatal
>       "08" -> DBFatal
>       _ -> DBError
>   throwDB (ec s e m)

> errorSqlState :: Int -> (String, String)
> errorSqlState 0  = ("00", "000")
> {-
> errorSqlState 1  = ("42", "000") -- sql error or missing database
> errorSqlState 2  = ("XX", "000") -- internal error
> errorSqlState 3  = ("42", "501") -- insufficient privileges/permission denied
> errorSqlState 4  = ("38", "000") -- callback requested abort
> errorSqlState 5  = ("58", "030") -- database file locked
> errorSqlState 6  = ("55", "006") -- table locked
> errorSqlState 7  = ("53", "200") -- malloc failed
> errorSqlState 8  = ("25", "006") -- can't write readonly database
> errorSqlState 9  = ("57", "014") -- query cancelled (interrupt)
> errorSqlState 10 = ("58", "030") -- io error
> errorSqlState 11 = ("58", "030") -- corrupt file
> errorSqlState 12 = ("42", "704") -- internal: object not found
> errorSqlState 13 = ("53", "100") -- database full
> errorSqlState 14 = ("58", "030") -- can't open database file
> errorSqlState 15 = ("55", "000") -- lock protocol error
> errorSqlState 16 = ("22", "000") -- internal: empty table
> errorSqlState 17 = ("42", "000") -- schema changed
> errorSqlState 18 = ("54", "000") -- row too big
> errorSqlState 19 = ("23", "000") -- constraint violation
> errorSqlState 20 = ("42", "804") -- data type mismatch
> errorSqlState 21 = ("39", "000") -- library used incorrectly
> errorSqlState 22 = ("58", "030") -- unsupported OS feature on host
> errorSqlState 23 = ("42", "501") -- authorisation denied
> -}
> errorSqlState _  = ("01", "000") -- unspecified error

|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = catchPG action convertAndRethrow


--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

We don't need much in an PostgreSQL Session record.
Session objects are created by 'connect'.

> newtype Session = Session { dbHandle :: DBHandle }

> data ConnectAttr = 
>    CAhost String
>  | CAhostaddr String
>  | CAport String
>  | CAdbname String
>  | CAuser String
>  | CApassword String
>  | CAconnect_timeout Int
>  | CAoptions String
>  | CAsslmode String
>  | CAservice String
>           
> connect :: [ConnectAttr] -> ConnectA Session
> connect attrs = ConnectA $ convertEx $ do
>   db <- DBAPI.openDb (unwords $ map encode attrs)
>   DBAPI.disableNoticeReporting db
>   return (Session db)
>  where 
>   -- process attributes into a string name=value
>   encode (CAhost s)            = "host=" ++ enc s
>   encode (CAhostaddr s)        = "hostaddr=" ++ enc s
>   encode (CAport s)            = "port=" ++ enc s
>   encode (CAdbname s)          = "dbname=" ++ enc s
>   encode (CAuser s)            = "user=" ++ enc s
>   encode (CApassword s)        = "password=" ++ enc s
>   encode (CAconnect_timeout i) = "connect_timeout=" ++ show i
>   encode (CAoptions s)         = "options=" ++ enc s
>   encode (CAsslmode s)         = "sslmode=" ++ enc s
>   encode (CAservice s)         = "service=" ++ enc s
>   enc s = "'" ++ qu  s ++ "'"
>   qu s = case break ( \c -> c == '\'' || c == '"' ) s of
>           (s,"") -> s
>           (s,(c:t))  -> s ++ ('\\' : c : qu t)


> isolationLevelText ReadUncommitted = "read uncomitted"
> isolationLevelText ReadCommitted = "read comitted"
> isolationLevelText RepeatableRead = "repeatable read"
> isolationLevelText Serialisable = "serializable"
> isolationLevelText Serializable = "serializable" 


> instance ISession Session where
>   disconnect sess = convertEx $ DBAPI.closeDb (dbHandle sess)
>   beginTransaction sess isolation = do
>     executeCommand sess (sql ("begin isolation level " ++ isolationLevelText isolation)) >> return ()
>   commit sess   = executeCommand sess (sql "commit") >> return ()
>   rollback sess = executeCommand sess (sql "rollback") >> return ()


--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------

The simplest kind of a statement: no tuning parameters, all default,
little overhead

> newtype QueryString = QueryString String
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand s (QueryString str) = executeCommand s str

> instance Command String Session where
>   executeCommand s str = do
>     (_,ntuple_str,_) <- convertEx $ DBAPI.nqExec (dbHandle s) str
>     return $ if ntuple_str == "" then 0 else read ntuple_str

> instance Command BoundStmt Session where
>   executeCommand s (BoundStmt (rs, count) _) = return count



|At present the only resource tuning we support is the number of rows
prefetched by the FFI library.
We use a record to (hopefully) make it easy to add other 
tuning parameters later.

> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 0  -- get it all at once


Simple prepared statement: the analogue of QueryString. It is useful
for DDL and DML statements, and for simple queries (that is, queries
that do not need cursors and result in a small enough dataset -- because
it will be fetched entirely in one shot).

The data constructor is not exported.

> data PreparedStmt = PreparedStmt { stmtName :: String }

> prepareStmt ::
>   String -> QueryString -> [DBAPI.Oid] -> PreparationA Session PreparedStmt
> prepareStmt [] _ _ = error "Prepared statement name must be non-empty"
> prepareStmt name (QueryString str) types = 
>   PreparationA (\sess -> 
>     convertEx $ DBAPI.stmtPrepare (dbHandle sess) name (DBAPI.substituteBindPlaceHolders str) types
>       >>= return . PreparedStmt)


|bindType is useful when constructing the list of Oids for stmtPrepare.
You don't need to pass the actual bind values, just dummy values
of the same type (the value isn't used, so undefined is OK here).

> bindType v = DBAPI.pgTypeOid v

> data BoundStmt = BoundStmt (ResultSetHandle, Int) String


The bindRun method returns a BoundStmt,
which contains just the result-set (and row count).

How do we bind a statement that will use prefetching?
We can't, because the prefetching code must surround the query text
with a cursor declaration.

Peraps we could use the same code, but have the no-prefetch case
create an empty advance action...

> type BindObj = DBAPI.PGBindVal

> instance IPrepared PreparedStmt Session BoundStmt BindObj where
>   bindRun sess stmt@(PreparedStmt stmt'name) bas action = do
>     let params = map (\(BindA ba) -> ba sess stmt) bas
>     rs <- convertEx $ DBAPI.stmtExec (dbHandle sess) stmt'name params
>     action (BoundStmt rs stmt'name)
>   destroyStmt sess stmt = do
>     executeCommand sess ("deallocate \"" ++ stmtName stmt ++ "\"")
>     return ()



-- Serialization (binding)

> makeBindAction x = BindA (\_ _ -> DBAPI.newBindVal x)

> instance DBBind (Maybe String) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int64) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Float) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Double) Session PreparedStmt BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe a) Session PreparedStmt BindObj
>     => DBBind a Session PreparedStmt BindObj where
>   bindP x = bindP (Just x)

The default instance, uses generic Show

> instance (Show a) => DBBind (Maybe a) Session PreparedStmt BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")

--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data SubQuery = SubQuery
>   { stmtHandle :: ResultSetHandle
>   , ntuples  :: Int  -- number of tuples to process in this subquery
>   , curr'row :: Int  -- current row, one-based. Should increment before use
>   }


FIXME: need to save cursor name and deallocate when query done?

> data Query = Query
>   { subquery :: IORef SubQuery
>   , advance'action :: Maybe (IO (ResultSetHandle, Int))
>   , cleanup'action :: Maybe (IO ())
>   , session :: Session
>   }

The following function creates the Query record. It has a few
decisions to make:
 -- should we prepare a statement or do execImm?
 -- should we use a cursor (better for large queries) or obtain
    all data in one shot?
    The use of cursor means we must be in a transaction.
 -- what is the name of the prepared statement? 
    Potentially, we should be able to execute a previously prepared
    statement...

Currently, if prefetchRowCount is 0 or the query is
not tuned, we use execImm. Otherwise,
we open a cursor and advance it by prefetchRowCount step.
We use anonymous prepared statement name.

The function commence'query also fetches some data (even if it turns
out 0 rows) so that later on, we could determine the type of data in columns
and prepare the buffers accordingly. We don't do that at the moment.


> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = makeQuery sess sqltext

> instance Statement String Session Query where
>   makeQuery sess sqltext = commence'query'simple sess sqltext []


Simple prepared statements.
Query has been executed and result-set is in handle.

> instance Statement BoundStmt Session Query where
>   makeQuery sess (BoundStmt (handle,ntuples) stmtName) = do
>     sqr <- newIORef $ SubQuery handle ntuples 0
>     return Query
>       { subquery = sqr
>       , advance'action = Nothing
>       , cleanup'action = Nothing
>       , session = sess
>       }


Statements with resource usage

> data QueryStringTuned = QueryStringTuned QueryResourceUsage String [BindA Session PreparedStmt BindObj]

> prefetch count sql parms = QueryStringTuned (QueryResourceUsage count) sql parms

> instance Statement QueryStringTuned Session Query where
>   makeQuery sess (QueryStringTuned resource_usage sqltext bas) = do
>     let params = map (\(BindA ba) -> ba sess undefined) bas
>     commence'query sess resource_usage sqltext params



> default'cursor'name = "takusenp"
> prepared'statement'name = "" -- meaning anonymous

> commence'query'simple sess sqltext params = do
>   subq <- create'subq sess $
>     convertEx $ DBAPI.stmtExecImm (dbHandle sess) (DBAPI.substituteBindPlaceHolders sqltext) params
>   sqr <- newIORef subq
>   return Query
>     { subquery = sqr
>     , advance'action = Nothing
>     , cleanup'action = Nothing
>     , session = sess
>     }

> commence'query sess resourceUsage sqltext params
>     | QueryResourceUsage{prefetchRowCount = 0} <- resourceUsage
>              = commence'query'simple sess sqltext params

Now, prepare and open the cursor

> commence'query sess resourceUsage sqltext params = do
>   let cursor = default'cursor'name
>   let q = "DECLARE " ++ cursor ++ " NO SCROLL CURSOR FOR " ++ sqltext
>   pstmt <- convertEx $ DBAPI.execCommand (dbHandle sess)
>     (DBAPI.substituteBindPlaceHolders q) params
>   let fetchq = "FETCH FORWARD " ++ (show $ prefetchRowCount resourceUsage) ++ " FROM " ++ cursor
>   sn <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) prepared'statement'name fetchq []
>   let advanceA = convertEx $ DBAPI.stmtExec0 (dbHandle sess) sn
>   let cleanupA =
>         convertEx $ DBAPI.nqExec (dbHandle sess) ("CLOSE " ++ cursor) >> return ()
>   subq <- create'subq sess advanceA
>   sqr <- newIORef subq
>   return Query
>     { subquery = sqr
>     , advance'action = Just advanceA
>     , cleanup'action = Just cleanupA
>     , session = sess
>     }

> create'subq sess action = do
>     (stmt,ntuples) <- action
>     return $ SubQuery stmt ntuples 0
> destroy'subq sess subq = convertEx $ DBAPI.stmtFinalise (stmtHandle subq)

  
> instance IQuery Query Session ColumnBuffer where
>
>   destroyQuery query = do
>     subq <- readIORef (subquery query)
>     destroy'subq (session query) subq
>     maybe (return ()) id (cleanup'action query)
>
>   fetchOneRow query = do
>     let sess  = session query
>     subq'  <- readIORef (subquery query)
>     let subq = subq' { curr'row = succ (curr'row subq') }
>     if ntuples subq == 0
>        then return False
>        else if curr'row subq > ntuples subq 
>        then maybe
>              (return False)                -- no advance action: we're done
>              (\action -> destroy'subq sess subq   >>
>                           create'subq  sess action >>=
>                           (writeIORef (subquery query)) >>
>                           fetchOneRow query)
>               (advance'action query)
>        else writeIORef (subquery query) subq >> return True
>
>   -- want to add a counter, so we can support this properly
>   currentRowNum q = readIORef (subquery q) >>= return . curr'row
>
>   freeBuffer q buffer = return ()

--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------

|There aren't really Buffers to speak of with PostgreSQL,
so we just record the position of each column.

> data ColumnBuffer = ColumnBuffer { colPos :: Int }

> buffer_pos q buffer = do 
>   row <- currentRowNum q
>   return (row, colPos buffer)

An auxiliary function: buffer allocation

> allocBuffer q colpos = return $ ColumnBuffer { colPos = colpos }

 nullIf :: Bool -> a -> Maybe a
 nullIf test v = if test then Nothing else Just v


> bufferToAny fn query buffer = do
>   subq <- readIORef (subquery query)
>   ind <- DBAPI.colValNull (stmtHandle subq) (curr'row subq) (colPos buffer)
>   if ind then return Nothing
>     else 
>       fn (stmtHandle subq) (curr'row subq) (colPos buffer)
>         >>= return . Just

> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValString

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValInt

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValDouble

> instance DBType (Maybe Float) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValFloat

> instance DBType (Maybe Int64) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValInt64

|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) $ fetchCol q buffer


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _  = allocBufferFor (undefined::String)
>   fetchCol q buffer = do
>     v <- bufferToAny DBAPI.colValString q buffer
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing
