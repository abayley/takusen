
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
>   ( Session, connect, disconnect, ConnectAttr(..) )
> where


> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Database.PostgreSQL.PGFunctions
>   (DBHandle, StmtHandle, PGException(..), catchPG, throwPG)
> import qualified Database.PostgreSQL.PGFunctions as DBAPI
> import Database.Enumerator
> import Control.Monad.Trans
> import Control.Monad.Reader
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
> convertAndRethrow (PGException errcode msg) = throwDB (DBError errcode msg)

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
> connect :: [ConnectAttr] -> IO Session
> connect attrs = convertEx $ do
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

> disconnect :: Session -> IO ()
> disconnect session = convertEx $ DBAPI.closeDb (dbHandle session)


> test_connect host = do
>  db <- connect [CAhostaddr host,CAuser "oleg",CAdbname "test"]
>  putStrLn $ "Database name: " ++ (show (DBAPI.fPQdb (dbHandle db)))
>  disconnect db
>  putStrLn "Done"


> type SessionM = ReaderT Session IO

> instance MonadSession SessionM IO Session where
>   runSession = flip runReaderT
>   getSession = ask
>
>   beginTransaction isolation = executeDDL "begin"
>
>   commit = executeDDL "commit"
>
>   rollback = executeDDL "rollback"
>   executeDML cmdText = do
>     sess <- ask
>     (_,ntuple_str,_) <- liftIO $ convertEx $ 
>		                DBAPI.nqExec (dbHandle sess) cmdText
>     return $ if ntuple_str == "" then 0 else read ntuple_str
>
>   executeDDL cmdText = do
>     sess <- ask
>     _ <- liftIO $ convertEx $ DBAPI.nqExec (dbHandle sess) cmdText
>     return ()

> test_ddl :: String -> IO ()
> test_ddl host = do
>  db <- connect [CAhostaddr host,CAuser "oleg",CAdbname "test"]
>  putStrLn "Connected"
>  runSession db $ do
>                   executeDDL "create table tdual (dummy text)"
>                   executeDML "insert into tdual values ('aaa')" >>= 
>		       liftIO . print
>                   executeDDL "drop table tdual"
>  disconnect db
>  putStrLn "Done"

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

Currently, if prefetchRowCount is 0, we use execImm. Otherwise,
we open a cursor and advance it by prefetchRowCount step.
We use anonymous prepared statement name.

The function commence'query also fetches some data (even if it turns
out 0 rows) so that later on, we could determine the type of data in columns
and prepare the buffers accordingly. We don't do that at the moment.


> default'cursor'name = "takusenp"
> prepared'statement'name = "" -- meaning anonymous

> commence'query sess resourceUsage sqltext 
>     | QueryResourceUsage{prefetchRowCount = 0} <- resourceUsage
>	   = do
>	       subq <- create'subq sess
>	                 (convertEx $ 
>			    DBAPI.stmtExecImm (dbHandle sess) sqltext)
>              sqr <- liftIO $ newIORef subq
>	       return Query {subquery = sqr,
>		             advance'action = Nothing,
>		             cleanup'action = Nothing,
>		             session = sess}

Now, prepare and open the cursor

> commence'query sess resourceUsage sqltext =
>     do
>     let cursor = default'cursor'name
>     let q = "DECLARE " ++ cursor ++ " NO SCROLL CURSOR FOR " ++ sqltext
>     executeDDL q
>     let fetchq = "FETCH FORWARD " ++ (show $ prefetchRowCount resourceUsage)
>	           ++ " FROM " ++ cursor
>     sn <- liftIO $ convertEx $ DBAPI.stmtPrepare (dbHandle sess) 
>                                   prepared'statement'name fetchq
>     let advanceA = convertEx $ DBAPI.stmtExec0 (dbHandle sess) sn
>     let cleanupA = convertEx $ DBAPI.nqExec (dbHandle sess) 
>	                                ("CLOSE " ++ cursor)
>		     >> return ()
>     subq <- create'subq sess advanceA
>     sqr <- liftIO $ newIORef subq
>     return Query {subquery = sqr,
>	            advance'action = Just advanceA,
>	            cleanup'action = Just cleanupA,
>	            session = sess}

> create'subq sess action = do
>     (stmt,ntuples) <- liftIO $ action
>     return $ SubQuery stmt ntuples 0
> destroy'subq sess subq = 
>     liftIO $ convertEx $ DBAPI.stmtFinalise (stmtHandle subq)

  

> type PGMonadQuery = ReaderT Query SessionM

> instance MonadQuery PGMonadQuery SessionM Query ColumnBuffer where
>
>   runQuery m query = runReaderT m query
>
>   getQuery = ask
>
>   makeQuery sqltext resourceUsage = do
>     sess <- getSession
>     commence'query sess resourceUsage sqltext 
>
>   execQuery query = return ()
>
>   destroyQuery query = do
>     sess <- getSession
>     subq <- liftIO $ readIORef (subquery query)
>     destroy'subq sess subq
>     maybe (return ()) liftIO (cleanup'action query)
>
>   fetch1Row = do
>     query <- getQuery
>     sess  <- lift $ getSession
>     subq'  <- liftIO $ readIORef (subquery query)
>     let subq = subq' { curr'row = succ (curr'row subq') }
>     if ntuples subq == 0
>        then return False
>        else if curr'row subq > ntuples subq 
>        then maybe
>              (return False)		-- no advance action: we're done
>              (\action -> destroy'subq sess subq   >>
>	                   create'subq  sess action >>=
>	                   (liftIO . writeIORef (subquery query)) >>
>	                   fetch1Row)
>	       (advance'action query)
>        else liftIO $ writeIORef (subquery query) subq >>
>             return True

>
>   allocBuffer _ colpos = do
>     q <- getQuery
>     return ColumnBuffer {colPos = colpos, query = q}
>
>   columnPosition buffer = return (colPos buffer)
>
>   -- want to add a counter, so we can support this properly
>   currentRowNum = return 0
>
>   freeBuffer buffer = return ()




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



|There aren't really Buffers to speak of with PostgreSQL,
so we just record the position of each column.
We also keep a reference to the Query which owns the buffer,
as we need it to get column values.

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   , query :: Query
>   }


> nullIf :: Bool -> a -> Maybe a
> nullIf test v = if test then Nothing else Just v

> bufferToString buffer = do
>   subq <- liftIO $ readIORef (subquery (query buffer))
>   ind  <- liftIO $ DBAPI.colValNull (stmtHandle subq) (curr'row subq) 
>                                     (colPos buffer)
>   if ind then return Nothing
>      else 
>        liftIO$ DBAPI.colValString (stmtHandle subq) (curr'row subq) 
>	                            (colPos buffer)
>        >>= return . Just

> {-
> bufferToInt buffer = do
>   v <- liftIO$ DBAPI.colValInt (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToDouble buffer = do
>   v <- liftIO$ DBAPI.colValDouble (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> nullDatetimeInt64 :: Int64
> nullDatetimeInt64 = 99999999999999

> bufferToDatetime :: (MonadIO m) => ColumnBuffer -> m (Maybe CalendarTime)
> bufferToDatetime buffer = do
>   v <- liftIO$ DBAPI.colValInt64 (stmtHandle (query buffer)) (colPos buffer)
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
>   liftIO $ bindMaybe (dbHandle sess) (stmtHandle query) pos val

If we have a null datetime, convert it to 99999999999999, rather than 0.
This ensures that dates have the same sorting behaviour as SQL,
which is to have nulls come last in the sort order.

> bindDatetime :: DBHandle -> StmtHandle -> Int -> Maybe CalendarTime -> IO ()
> bindDatetime db stmt pos mbval =  convertEx $
>   case mbval of
>     Nothing -> DBAPI.bindInt64 db stmt pos nullDatetimeInt64
>     Just val -> stmtBind db stmt pos val
> -}


> instance DBType (Maybe String) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToString buffer
>   bindPos p v = return () -- bindMb

> {-
> instance DBType (Maybe Int) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToInt buffer
>   bindPos = bindMb

> instance DBType (Maybe Double) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDouble buffer
>   bindPos = bindMb

> instance DBType (Maybe CalendarTime) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDatetime buffer
>   bindPos pos val = do
>     sess <- lift getSession
>     query <- getQuery
>     liftIO $ bindDatetime (dbHandle sess) (stmtHandle query) pos val
> -}

|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) PGMonadQuery ColumnBuffer
>     => DBType a PGMonadQuery ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol buffer = throwIfDBNull buffer fetchCol
>   bindPos pos val = bindPos pos (Just val)


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) PGMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = do
>     v <- liftIO$ bufferToString buffer
>     return $ liftM read v
>   bindPos pos val = do
>     sess <- lift getSession
>     query <- getQuery
>     let justString = maybe Nothing (Just . show) val
>     return () -- liftIO $ bindMaybe (dbHandle sess) (stmtHandle query) pos justString

