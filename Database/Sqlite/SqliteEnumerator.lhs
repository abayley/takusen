
|
Module      :  Database.Sqlite.SqliteEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Sqlite implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-undecidable-instances #-}
> {-# OPTIONS -fallow-overlapping-instances #-}

> module Database.Sqlite.SqliteEnumerator
>   ( Session, connect, disconnect )
> where


> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Database.Sqlite.SqliteFunctions
>   (DBHandle, StmtHandle, SqliteException(..), catchSqlite, throwSqlite)
> import qualified Database.Sqlite.SqliteFunctions as DBAPI
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
and never SqliteExceptions.
We don't need wrappers for the colValXxx functions
because they never throw exceptions.


> convertAndRethrow :: SqliteException -> IO a
> convertAndRethrow (SqliteException errcode msg) = do
>   throwDB (DBError errcode msg)
>   return undefined

|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = catchSqlite action convertAndRethrow

> executeSql :: DBHandle -> String -> IO Int
> executeSql db sqltext = convertEx $ do
>   rc <- DBAPI.stmtExec db sqltext
>   return rc

> prepareStmt :: DBHandle -> String -> IO StmtHandle
> prepareStmt db sqltext = convertEx $ DBAPI.stmtPrepare db sqltext

> fetchRow :: DBHandle -> StmtHandle -> IO CInt
> fetchRow db stmt = convertEx $ DBAPI.stmtFetch db stmt

> finaliseStmt :: DBHandle -> StmtHandle -> IO ()
> finaliseStmt db stmt = convertEx $ DBAPI.stmtFinalise db stmt


--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

|We don't need much in an Sqlite Session record.
Session objects are created by 'connect'.

> data Session = Session { dbHandle :: DBHandle }

> connect :: String -> String -> String -> IO Session
> connect user pswd dbname = convertEx $ do
>   db <- DBAPI.openDb dbname
>   return (Session db)

> disconnect :: Session -> IO ()
> disconnect session = convertEx $ DBAPI.closeDb (dbHandle session)

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
>
>   executeDML cmdText = do
>     sess <- ask
>     rc <- liftIO $ executeSql (dbHandle sess) cmdText
>     return rc
>
>   executeDDL cmdText = do
>     _ <- executeDML cmdText
>     return ()

--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data Query = Query
>   { stmtHandle :: StmtHandle
>   , queryResourceUsage :: QueryResourceUsage
>   , session :: Session
>   }


> type SqliteMonadQuery = ReaderT Query SessionM


> instance MonadQuery SqliteMonadQuery SessionM Query ColumnBuffer where
>
>   runQuery m query = runReaderT m query
>
>   getQuery = ask
>
>   makeQuery sqltext resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ prepareStmt (dbHandle sess) sqltext
>     return $ Query stmt resourceUsage sess
>
>   destroyQuery query = do
>     sess <- getSession
>     liftIO $ finaliseStmt (dbHandle sess) (stmtHandle query)
>
>   fetch1Row = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ fetchRow (dbHandle sess) (stmtHandle query)
>     --if rc == DBAPI.sqliteDONE then return False else return True
>     return (not (rc == DBAPI.sqliteDONE))
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



|There aren't really Buffers to speak of with Sqlite,
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
>   v <- liftIO$ DBAPI.colValString (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToInt buffer = do
>   v <- liftIO$ DBAPI.colValInt (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToDouble buffer = do
>   v <- liftIO$ DBAPI.colValDouble (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToDatetime :: (MonadIO m) => ColumnBuffer -> m (Maybe CalendarTime)
> bufferToDatetime buffer = do
>   v <- liftIO$ DBAPI.colValInt64 (stmtHandle (query buffer)) (colPos buffer)
>   return (nullIf (v == 0) (makeCalTime v))



> class SqliteBind a where
>   stmtBind :: DBHandle -> StmtHandle -> Int -> a -> IO ()

> instance SqliteBind Int where stmtBind = DBAPI.bindInt
> instance SqliteBind String where stmtBind = DBAPI.bindString
> instance SqliteBind Double where stmtBind = DBAPI.bindDouble
> instance SqliteBind CalendarTime where
>   stmtBind db stmt pos val =
>     DBAPI.bindInt64 db stmt pos (makeInt64 val)

> bindMaybe :: (SqliteBind a)
>   => DBHandle -> StmtHandle -> Int -> Maybe a -> IO ()
> bindMaybe db stmt pos mv = convertEx $
>   case mv of
>     Nothing -> DBAPI.bindNull db stmt pos
>     Just val -> stmtBind db stmt pos val

> bindMb pos val = do
>     sess <- lift getSession
>     query <- getQuery
>     liftIO $ bindMaybe (dbHandle sess) (stmtHandle query) pos val


> instance DBType (Maybe String) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToString buffer
>   bindPos = bindMb

> instance DBType (Maybe Int) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToInt buffer
>   bindPos = bindMb

> instance DBType (Maybe Double) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDouble buffer
>   bindPos = bindMb

> instance DBType (Maybe CalendarTime) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDatetime buffer
>   bindPos = bindMb


|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) SqliteMonadQuery ColumnBuffer
>     => DBType a SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol buffer = throwIfDBNull buffer fetchCol
>   bindPos pos val = bindPos pos (Just val)


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = do
>     v <- liftIO$ bufferToString buffer
>     return $ maybe Nothing (Just . read) v
>   bindPos pos val = do
>     sess <- lift getSession
>     query <- getQuery
>     let justString = maybe Nothing (Just . show) val
>     liftIO $ bindMaybe (dbHandle sess) (stmtHandle query) pos justString
