
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

> module Database.Sqlite.SqliteEnumerator where


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

> data Session = Session { dbHandle :: DBHandle }

> connect :: String -> String -> String -> IO Session
> connect user pswd dbname = convertEx $ do
>   db <- DBAPI.openDb dbname
>   return (Session db)

> disconnect :: Session -> IO ()
> disconnect session = convertEx $ DBAPI.closeDb (dbHandle session)



> instance MonadSession (ReaderT Session IO) IO Session where
>   runSession = runReaderT
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
>   }


> type SqliteMonadQuery = ReaderT Query (ReaderT Session IO)


> instance MonadQuery SqliteMonadQuery (ReaderT Session IO) Query ColumnBuffer where
>
>   -- so, doQuery is essentially the result of applying lfold_nonrec_to_rec
>   -- to doQuery1Maker
>   doQuery = doQueryTuned defaultResourceUsage
>
>   doQueryTuned resourceUsage sqltext iteratee seed = do
>     (lFoldLeft, finalizer) <- doQuery1Maker sqltext iteratee resourceUsage
>     catchReaderT (fix lFoldLeft iteratee seed)
>       (\e -> do
>         finalizer
>         liftIO $ throwIO e
>       )
>
>   openCursor = openCursorTuned defaultResourceUsage
>
>   -- This is like 
>   -- lfold_nonrec_to_stream lfold' =
>   -- from the fold-stream.lhs paper:
>   openCursorTuned resourceUsage sqltext iteratee seed = do
>     ref <- liftIO$ newIORef (seed, Nothing)
>     (lFoldLeft, finalizer) <- doQuery1Maker sqltext iteratee resourceUsage
>     let update v = liftIO $ modifyIORef ref (\ (_, f) -> (v, f))
>     let
>       close finalseed = do
>         liftIO$ modifyIORef ref (\_ -> (finalseed, Nothing))
>         finalizer
>         return $ DBCursor ref
>     let
>       k' fni seed' = 
>         let
>           k fni' seed'' = do
>             let k'' flag = if flag then k' fni' seed'' else close seed''
>             liftIO$ modifyIORef ref (\_->(seed'', Just k''))
>             return seed''
>         in do
>           liftIO$ modifyIORef ref (\_ -> (seed', Nothing))
>           do {lFoldLeft k fni seed' >>= update}
>           return $ DBCursor ref
>     k' iteratee seed
>
>   getQuery = ask
>
>   makeQuery sqltext resourceUsage = do
>     sess <- getSession
>     stmt <- liftIO $ prepareStmt (dbHandle sess) sqltext
>     return $ Query stmt resourceUsage
>
>   doQuery1Maker sqltext iteratee resourceUsage = do
>     sess <- getSession
>     query <- makeQuery sqltext resourceUsage
>     let inQuery m = runReaderT m query
>     buffers <- inQuery $ allocBuffers iteratee 1
>     let
>       finaliser = liftIO $ finaliseStmt (dbHandle sess) (stmtHandle query)
>       hFoldLeft self iteratee seedVal = 
>         runfetch inQuery finaliser buffers self iteratee seedVal
>     return (hFoldLeft, inQuery finaliser)
>
>   fetch1 = do
>     query <- getQuery
>     sess <- lift getSession
>     rc <- liftIO $ fetchRow (dbHandle sess) (stmtHandle query)
>     if rc == DBAPI.sqliteDONE then return False else return True
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
>     --, ctWDay = Sunday
>     --, ctYDay = -1
>     , ctTZName = "UTC"
>     , ctTZ = 0
>     , ctIsDST = False
>     }

|There aren't really Buffers to speak of with Sqlite,
so we just record the position of each column.
We also keep a reference to the Query which owns the buffer,
as we need it to get column values.

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   , query :: Query
>   }



> bufferToString buffer = do
>   v <- liftIO$ DBAPI.colValString (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToInt buffer = do
>   v <- liftIO$ DBAPI.colValInt (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToDouble buffer = do
>   v <- liftIO$ DBAPI.colValDouble (stmtHandle (query buffer)) (colPos buffer)
>   return (Just v)

> bufferToDatetime buffer = do
>   v <- liftIO$ DBAPI.colValInt64 (stmtHandle (query buffer)) (colPos buffer)
>   return (Just (makeCalTime v))




> instance DBType (Maybe String) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToString buffer

> instance DBType (Maybe Int) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToInt buffer

> instance DBType (Maybe Double) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDouble buffer

> instance DBType (Maybe CalendarTime) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = liftIO$ bufferToDatetime buffer


A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ n = allocBuffer undefined n
>   fetchCol buffer = do
>     v <- liftIO$ bufferToString buffer
>     case v of
>       Just s -> return (Just (read s))
>       Nothing -> return Nothing


This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) SqliteMonadQuery ColumnBuffer
>     => DBType a SqliteMonadQuery ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol buffer = throwIfDBNull buffer fetchCol
