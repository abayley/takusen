
|
Module      :  Database.Sqlite.SqliteEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Sqlite implementation of Database.Enumerator.

> {-

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
> convertAndRethrow (SqliteException e m) = do
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
> errorSqlState _  = ("01", "000") -- unspecified error

|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = catchSqlite action convertAndRethrow

|Returns number of rows modified.

> executeSql :: DBHandle -> String -> IO Int
> executeSql db sqltext = convertEx $ do
>   rc <- DBAPI.stmtExec db sqltext
>   rows <- DBAPI.stmtChanges db
>   return rows

> prepareStmt :: DBHandle -> String -> IO StmtHandle
> prepareStmt db sqltext = convertEx $ DBAPI.stmtPrepare db sqltext

> fetchRow :: DBHandle -> StmtHandle -> IO CInt
> fetchRow db stmt = convertEx $ DBAPI.stmtFetch db stmt

> finaliseStmt :: DBHandle -> StmtHandle -> IO ()
> finaliseStmt db stmt = convertEx $ DBAPI.stmtFinalise db stmt


--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

We don't need much in an Sqlite Session record.
Session objects are created by 'connect'.

> data Session = Session { dbHandle :: DBHandle }

> data PreparedStatement = PreparedStatement
>   { stmtHandle :: StmtHandle
>   , stmtSession :: Session
>   , stmtResourceUsage :: QueryResourceUsage
>   }

> connect :: String -> IO Session
> connect dbname = convertEx $ do
>   db <- DBAPI.openDb dbname
>   return (Session db)

> disconnect :: Session -> IO ()
> disconnect session = convertEx $ DBAPI.closeDb (dbHandle session)

> type SessionM = ReaderT Session IO

> instance MonadSession SessionM IO Session PreparedStatement where
>   runSession = flip runReaderT
>   getSession = ask
>
>   beginTransaction isolation = executeDDL "begin" []
>
>   commit = do
>     sess <- getSession
>     liftIO $ executeSql (dbHandle sess) "commit"
>     return ()
>
>   rollback = do
>     sess <- getSession
>     liftIO $ executeSql (dbHandle sess) "rollback"
>     return ()
>
>   withStatement sqlText resourceUsage action = do
>     sess <- getSession
>     stmt <- liftIO $ prepareStmt (dbHandle sess) sqlText
>     catchReaderT (do
>         v <- action (PreparedStatement stmt sess resourceUsage)
>         liftIO $ finaliseStmt (dbHandle sess) stmt
>         return v
>       )
>       (\e -> do
>           liftIO $ finaliseStmt (dbHandle sess) stmt
>           liftIO $ throwIO e
>           return undefined
>       )
>
>   bindParameters stmt bindActions = do
>     let nats :: [Int]; nats = [1..]
>     sequence_ $ map (\(p, ba) -> ba stmt p) (zip nats bindActions)
>
>   executeStatement stmt = do
>     _ <- liftIO $ fetchRow (dbHandle (stmtSession stmt)) (stmtHandle stmt)
>     liftIO $ DBAPI.stmtReset (dbHandle (stmtSession stmt)) (stmtHandle stmt)
>     liftIO $ DBAPI.stmtChanges (dbHandle (stmtSession stmt))
>
>   prepareStatement sqlText resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ prepareStmt (dbHandle sess) sqlText
>     return (PreparedStatement stmt sess resourceUsage)
>
>   freeStatement stmt = do
>         liftIO $ finaliseStmt (dbHandle (stmtSession stmt)) (stmtHandle stmt)
>
>   executeDML sqlText args = do
>     sess <- getSession
>     withStatement sqlText defaultResourceUsage $ \stmt -> do
>       bindParameters stmt args
>       executeStatement stmt
>
>   executeDDL sqlText args = do
>     _ <- executeDML sqlText args
>     return ()

--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data Query = Query
>   { queryStmt :: PreparedStatement
>   , queryCount :: IORef Int
>   }


> type QueryM = ReaderT Query SessionM


> instance MonadQuery QueryM SessionM PreparedStatement ColumnBuffer Query where
>
>   runQuery = flip runReaderT
>   getQuery = ask
>
>   makeQuery stmt bindacts = do
>     bindParameters stmt bindacts
>     count <- liftIO $ newIORef 0
>     return (Query stmt count)
>
>   fetchOneRow = do
>     query <- getQuery
>     sess <- lift getSession
>     count <- liftIO $ readIORef (queryCount query)
>     liftIO $ writeIORef (queryCount query) (count + 1)
>     rc <- liftIO $ fetchRow (dbHandle sess) (stmtHandle (queryStmt query))
>     return (not (rc == DBAPI.sqliteDONE))
>
>   allocBuffer _ colpos = do
>     q <- getQuery
>     return ColumnBuffer {colPos = colpos, colStmt = (queryStmt q)}
>
>   columnPosition buffer = return (colPos buffer)
>
>   currentRowNum = do
>     query <- getQuery
>     liftIO $ readIORef (queryCount query)
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
>   , colStmt :: PreparedStatement
>   }


> nullIf :: Bool -> a -> Maybe a
> nullIf test v = if test then Nothing else Just v

> bufferToString buffer =
>   liftIO$ DBAPI.colValString (stmtHandle (colStmt buffer)) (colPos buffer)

> bufferToInt buffer = do
>   v <- liftIO$ DBAPI.colValInt (stmtHandle (colStmt buffer)) (colPos buffer)
>   return (Just v)

> bufferToDouble buffer = do
>   v <- liftIO$ DBAPI.colValDouble (stmtHandle (colStmt buffer)) (colPos buffer)
>   return (Just v)

> nullDatetimeInt64 :: Int64
> nullDatetimeInt64 = 99999999999999

> bufferToDatetime :: (MonadIO m) => ColumnBuffer -> m (Maybe CalendarTime)
> bufferToDatetime buffer = do
>   v <- liftIO$ DBAPI.colValInt64 (stmtHandle (colStmt buffer)) (colPos buffer)
>   return (nullIf (v == 0 || v == nullDatetimeInt64) (makeCalTime v))



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

> bindMb (stmt::PreparedStatement) val pos = do
>   let sess = stmtSession stmt
>   liftIO $ bindMaybe (dbHandle sess) (stmtHandle stmt) pos val

If we have a null datetime, convert it to 99999999999999, rather than 0.
This ensures that dates have the same sorting behaviour as SQL,
which is to have nulls come last in the sort order.

> bindDatetime :: DBHandle -> StmtHandle -> Int -> Maybe CalendarTime -> IO ()
> bindDatetime db stmt pos mbval =  convertEx $
>   case mbval of
>     Nothing -> DBAPI.bindInt64 db stmt pos nullDatetimeInt64
>     Just val -> stmtBind db stmt pos val

> instance DBBind (Maybe String) SessionM PreparedStatement where
>   bindPos = bindMb

> instance DBBind (Maybe Int) SessionM PreparedStatement where
>   bindPos = bindMb

> instance DBBind (Maybe Double) SessionM PreparedStatement where
>   bindPos = bindMb

> instance DBBind (Maybe CalendarTime) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     let sess = stmtSession stmt
>     liftIO $ bindDatetime (dbHandle sess) (stmtHandle stmt) pos val

> instance DBBind (Maybe a) SessionM PreparedStatement
>     => DBBind a SessionM PreparedStatement where
>   bindPos stmt val pos = bindPos stmt (Just val) pos

> instance (Show a, Read a) => DBBind (Maybe a) SessionM PreparedStatement where
>   bindPos stmt val pos = do
>     let sess = stmtSession stmt
>     let justString = maybe Nothing (Just . show) val
>     liftIO $ bindMaybe (dbHandle sess) (stmtHandle stmt) pos justString



> instance DBType (Maybe String) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToString buffer

> instance DBType (Maybe Int) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToInt buffer

> instance DBType (Maybe Double) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDouble buffer

> instance DBType (Maybe CalendarTime) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDatetime buffer


|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) QueryM ColumnBuffer
>     => DBType a QueryM ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol buffer = throwIfDBNull buffer fetchCol


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) QueryM ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = do
>     v <- liftIO$ bufferToString buffer
>     return $ maybe Nothing (Just . read) v

> -}
