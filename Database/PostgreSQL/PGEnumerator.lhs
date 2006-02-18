
|
Module      :  Database.PostgreSQL.PGEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
PostgreSQL implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.PostgreSQL.PGEnumerator
>   -- Only the type constructor of Session is exported
>   -- (so the end user could write type signatures). 
>   ( Session, connect, ConnectAttr(..)
>    , sql
>    , QueryResourceUsage(..), sql_tuned
>   )
> where


> import Database.InternalEnumerator
> import Foreign.C
> import Control.Monad
> import Control.Exception (catchDyn, throwDyn, throwIO)
> import Database.PostgreSQL.PGFunctions
>   (DBHandle, StmtHandle, PGException(..), catchPG, throwPG)
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
> convertEx action = catchPG action (\e -> print e >> convertAndRethrow e)

> {-
> prepareStmt :: DBHandle -> String -> IO StmtHandle
> prepareStmt db sqltext = convertEx $ DBAPI.stmtPrepare db sqltext

> fetchRow :: DBHandle -> StmtHandle -> IO CInt
> fetchRow db stmt = convertEx $ DBAPI.stmtFetch db stmt

> -}

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
>	             (s,"") -> s
>                    (s,(c:t))  -> s ++ ('\\' : c : qu t)


> test_connect host = do
>  let (ConnectA ca) = connect [CAhostaddr host,CAuser "oleg",CAdbname "test"]
>  db <- ca
>  putStrLn $ "Database name: " ++ (show (DBAPI.fPQdb (dbHandle db)))
>  disconnect db
>  putStrLn "Done"


> instance ISession Session where
>   disconnect sess = convertEx $ DBAPI.closeDb (dbHandle sess)
>   beginTransaction sess isolation =
>	executeCommand sess (sql "begin") >> return ()
>   commit sess   = executeCommand sess (sql "commit") >> return ()
>   rollback sess = executeCommand sess (sql "rollback") >> return ()


--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------

The simplest kind of a statement: no tuning parameters, all default,
little overhead

> data QueryString = QueryString String
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand s (QueryString str)  = do
>     (_,ntuple_str,_) <- convertEx $ DBAPI.nqExec (dbHandle s) str
>     return $ if ntuple_str == "" then 0 else read ntuple_str

> test_ddl :: String -> IO ()
> test_ddl host = do
>  let (ConnectA ca) = connect [CAhostaddr host,CAuser "oleg",CAdbname "test"]
>  db <- ca
>  putStrLn "Connected"
>  do
>    executeCommand db (sql "create table tdual (dummy text)")
>    executeCommand db (sql "insert into tdual values ('aaa')") >>= print
>    executeCommand db (sql "drop table tdual")
>  disconnect db
>  putStrLn "Done"

|At present the only resource tuning we support is the number of rows
prefetched by the FFI library.
We use a record to (hopefully) make it easy to add other 
tuning parameters later.

> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 100



--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data SubQuery = SubQuery
>   { stmtHandle :: StmtHandle
>   , ntuples  :: Int  -- number of tuples to process in this subquery
>   , curr'row :: Int  -- current row, one-based. Should increment before use
>   }
> 
> data Query = Query
>   { subquery :: IORef SubQuery
>   , advance'action :: Maybe (IO (StmtHandle, Int))
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
>   makeQuery sess (QueryString sqltext) = commence'query'simple sess sqltext


-- Statements with resource usage

> data QueryStringTuned = QueryStringTuned QueryResourceUsage String
> sql_tuned resource_usage str = QueryStringTuned resource_usage str
>
> instance Statement QueryStringTuned Session Query where
>   makeQuery sess (QueryStringTuned resource_usage sqltext) = 
>	commence'query sess resource_usage sqltext


> default'cursor'name = "takusenp"
> prepared'statement'name = "" -- meaning anonymous

> commence'query'simple sess sqltext 
>     = do
>       subq <- create'subq sess
>	                 (convertEx $ 
>			    DBAPI.stmtExecImm (dbHandle sess) sqltext)
>	sqr <- newIORef subq
>       return Query {subquery = sqr,
>	              advance'action = Nothing,
>	              cleanup'action = Nothing,
>	              session = sess}

> commence'query sess resourceUsage sqltext 
>     | QueryResourceUsage{prefetchRowCount = 0} <- resourceUsage
>              = commence'query'simple sess sqltext

Now, prepare and open the cursor

> commence'query sess resourceUsage sqltext =
>     do
>     let cursor = default'cursor'name
>     let q = sql ("DECLARE " ++ cursor ++ " NO SCROLL CURSOR FOR " ++ sqltext)
>     executeCommand sess q
>     let fetchq = "FETCH FORWARD " ++ (show $ prefetchRowCount resourceUsage)
>	           ++ " FROM " ++ cursor
>     sn <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) 
>                          prepared'statement'name fetchq
>     let advanceA = convertEx $ DBAPI.stmtExec0 (dbHandle sess) sn
>     let cleanupA = convertEx $ DBAPI.nqExec (dbHandle sess) 
>	                                ("CLOSE " ++ cursor)
>		     >> return ()
>     subq <- create'subq sess advanceA
>     sqr <- newIORef subq
>     return Query {subquery = sqr,
>	            advance'action = Just advanceA,
>	            cleanup'action = Just cleanupA,
>	            session = sess}

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
>              (return False)		-- no advance action: we're done
>              (\action -> destroy'subq sess subq   >>
>	                   create'subq  sess action >>=
>	                   (writeIORef (subquery query)) >>
>	                   fetchOneRow query)
>	       (advance'action query)
>        else writeIORef (subquery query) subq >> return True
>
>   -- want to add a counter, so we can support this properly
>   currentRowNum q = return 0
>
>   freeBuffer q buffer = return ()

--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------

|There aren't really Buffers to speak of with PostgreSQL,
so we just record the position of each column.

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   }

> buffer_pos q buffer = 
>     do 
>     let col = colPos buffer
>     row <- currentRowNum q
>     return (row,col)

An auxiliary function: buffer allocation

> allocBuffer q colpos = do
>     return $ ColumnBuffer
>       { colPos = colpos
>       }


20040822073512
   10000000000 (10 ^ 10) * year
     100000000 (10 ^ 8) * month
       1000000 (10 ^ 6) * day
         10000  (10^4) * hour

Use quot and rem, /not/ div and mod,
so that we get sensible behaviour for -ve numbers.

> makeCalTime :: Int64 -> CalendarTime
> makeCalTime i =
>   let
>     year = (i `quot` 10000000000)
>     month = ((abs i) `rem` 10000000000) `quot` 100000000
>     day = ((abs i) `rem` 100000000) `quot` 1000000
>     hour = ((abs i) `rem` 1000000) `quot` 10000
>     minute = ((abs i) `rem` 10000) `quot` 100
>     second = ((abs i) `rem` 100)
>   in CalendarTime
>     { ctYear = fromIntegral year
>     , ctMonth = toEnum (fromIntegral month - 1)
>     , ctDay = fromIntegral day
>     , ctHour = fromIntegral hour
>     , ctMin = fromIntegral minute
>     , ctSec = fromIntegral second
>     , ctPicosec = 0
>     , ctWDay = Sunday
>     , ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }

> makeInt64 :: CalendarTime -> Int64
> makeInt64 ct =
>   let
>     yearm :: Int64
>     yearm = 10000000000
>   in  yearm * fromIntegral (ctYear ct)
>   + 100000000 * fromIntegral ((fromEnum (ctMonth ct) + 1))
>   + 1000000 * fromIntegral (ctDay ct)
>   + 10000 * fromIntegral (ctHour ct)
>   + 100 * fromIntegral (ctMin ct)
>   + fromIntegral (ctSec ct)





> nullIf :: Bool -> a -> Maybe a
> nullIf test v = if test then Nothing else Just v

> bufferToString query buffer = do
>   subq <- readIORef (subquery query)
>   ind  <- DBAPI.colValNull (stmtHandle subq) (curr'row subq) 
>                            (colPos buffer)
>   if ind then return Nothing
>      else 
>        DBAPI.colValString (stmtHandle subq) (curr'row subq) 
>	                    (colPos buffer)
>        >>= return . Just

> {-
> bufferToInt buffer = do
>   v <- DBAPI.colValInt (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToDouble buffer = do
>   v <- DBAPI.colValDouble (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> nullDatetimeInt64 :: Int64
> nullDatetimeInt64 = 99999999999999

> bufferToDatetime :: (MonadIO m) => ColumnBuffer -> m (Maybe CalendarTime)
> bufferToDatetime buffer = do
>   v <- DBAPI.colValInt64 (stmtHandle (query buffer)) (colPos buffer)
>   return (nullIf (v == 0 || v == nullDatetimeInt64) (makeCalTime v))



> class PGBind a where
>   stmtBind :: DBHandle -> StmtHandle -> Int -> a -> IO ()

> instance PGBind Int where stmtBind = DBAPI.bindInt
> instance PGBind String where stmtBind = DBAPI.bindString
> instance PGBind Double where stmtBind = DBAPI.bindDouble
> instance PGBind CalendarTime where
>   stmtBind db stmt pos val =
>     DBAPI.bindInt64 db stmt pos (makeInt64 val)

> bindMaybe :: (PGBind a)
>   => DBHandle -> StmtHandle -> Int -> Maybe a -> IO ()
> bindMaybe db stmt pos mv = convertEx $
>   case mv of
>     Nothing -> DBAPI.bindNull db stmt pos
>     Just val -> stmtBind db stmt pos val

> bindMb pos val = do
>   sess <- lift getSession
>   query <- getQuery
>   bindMaybe (dbHandle sess) (stmtHandle query) pos val

If we have a null datetime, convert it to 99999999999999, rather than 0.
This ensures that dates have the same sorting behaviour as SQL,
which is to have nulls come last in the sort order.

> bindDatetime :: DBHandle -> StmtHandle -> Int -> Maybe CalendarTime -> IO ()
> bindDatetime db stmt pos mbval =  convertEx $
>   case mbval of
>     Nothing -> DBAPI.bindInt64 db stmt pos nullDatetimeInt64
>     Just val -> stmtBind db stmt pos val
> -}

> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToString


> --   bindPos p v = return () -- bindMb

> {-
> instance DBType (Maybe Int) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = bufferToInt buffer
>   bindPos = bindMb

> instance DBType (Maybe Double) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = bufferToDouble buffer
>   bindPos = bindMb

> instance DBType (Maybe CalendarTime) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = bufferToDatetime buffer
>   bindPos pos val = do
>     sess <- lift getSession
>     query <- getQuery
>     bindDatetime (dbHandle sess) (stmtHandle query) pos val
> -}

|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) $ fetchCol q buffer

>  -- bindPos pos val = bindPos pos (Just val)


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _  = allocBufferFor (undefined::String)
>   fetchCol q buffer = do
>     v <- bufferToString q buffer
>     case v of
>       Just s -> return (Just (read s))
>       Nothing -> return Nothing
