
{-|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainers :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Abstract database interface, providing a left-fold enumerator
and cursor operations.
-}

> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Enumerator where

> import System.Time (CalendarTime)
> import Data.Dynamic (Typeable)
> import Data.IORef
> import Control.Monad.Trans
> import Control.Exception (catchDyn, throwDyn, throwIO, Exception)
> import qualified Control.Exception (catch)
> import Control.Monad.Reader


See Database.Enumerator.OCIEnumerator for instructions on adding new types.
You need to modify both this module and Database.Oracle.OCIEnumerator,
so I put the instructions in there.

> data DBColumnType =
>     DBTypeInt
>   | DBTypeString
>   | DBTypeDouble
>   | DBTypeDatetime

> data IsolationLevel =
>     ReadUncommitted
>   | ReadCommitted
>   | RepeatableRead
>   | Serialisable
>   | Serializable  -- for alternative spellers
>   deriving Show

> type BufferSize = Int
> type BufferHint = (BufferSize, DBColumnType)
> type Position = Int
> type IterResult seedType = Either seedType seedType
> type IterAct seedType = seedType -> IterResult seedType
> type QueryText = String
> type ColNum = Int
> type RowNum = Int



--------------------------------------------------------------------
-- Exceptions and handlers
--------------------------------------------------------------------

> data DBException =
>     DBError Int String
>   | DBUnexpectedNull RowNum ColNum
>   | DBNoData
>   deriving (Typeable, Show)

If we can't derive Typeable then the following code should do the trick:

dbExceptionTc :: TyCon
dbExceptionTc = mkTyCon "Database.Enumerator.DBException"
instance Typeable DBException where typeOf _ = mkAppTy dbExceptionTc []

> catchDB :: IO a -> (DBException -> IO a) -> IO a
> catchDB = catchDyn
> throwDB :: DBException -> a
> throwDB = throwDyn

This simple handler reports the error and swallows it
i.e. it doesn't propagate.

> basicDBExceptionReporter :: DBException -> IO ()
> basicDBExceptionReporter (DBError e m) = putStrLn m
> basicDBExceptionReporter (DBUnexpectedNull r c) = putStrLn $ "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
> basicDBExceptionReporter (DBNoData) = putStrLn "Fetch: no more data."

If you want to trap a specific error number, use this.
It passes anything else up.

> catchDBError :: Int -> IO a -> (DBException -> IO a) -> IO a
> catchDBError n action handler = catchDB action
>   (\dberror ->
>     case dberror of
>       DBError e m | e == n -> handler dberror
>       _ | otherwise -> throwDB dberror
>   )

> ignoreDBError :: Int -> IO a -> IO a
> ignoreDBError n action = catchDBError n action (\e -> return undefined)


These two functions let us catch (and rethrow) exceptions in the ReaderT monad.

> shakeReaderT :: ((ReaderT r m1 a1 -> m1 a1) -> m a) -> ReaderT r m a
> shakeReaderT f = ReaderT $ \r -> f (\lm -> runReaderT lm r)

> catchReaderT :: ReaderT r IO a -> (Control.Exception.Exception -> ReaderT r IO a) -> ReaderT r IO a
> catchReaderT m h = shakeReaderT $ \sinker -> Control.Exception.catch (sinker m) (sinker . h)


Useful utility function, for SQL weenies.

> ifNull :: Maybe a -> a -> a
> ifNull value subst = maybe subst id value


--------------------------------------------------------------------
-- Session monad.
--------------------------------------------------------------------


> class (MonadIO mb) => MonadSession m mb s | m -> mb, m -> s, mb s -> m where
>   runSession :: m a -> s -> mb a
>   getSession :: m s
>   beginTransaction :: IsolationLevel -> m ()
>   commit :: m ()
>   rollback :: m ()
>   -- insert/update/delete; returns number of rows affected
>   executeDML :: QueryText -> m Int
>   -- DDL (create table, etc)
>   executeDDL :: QueryText -> m ()
>   -- PL/SQL blocks? Not in generic interface. ODBC can execute procedures; maybe later...
>   --executeProc :: QueryText -> m ()


--------------------------------------------------------------------
-- Buffers and QueryIteratee.
--------------------------------------------------------------------

A type class for Buffers.

> class (Monad m) => Buffer m bufferType | m -> bufferType where
>   allocBuffer :: BufferHint -> Position -> m bufferType
>   columnPosition :: bufferType -> m Int
>   currentRowNum :: m Int
>   freeBuffer :: bufferType -> m ()
>   -- should these fetch functions be separated out?
>   fetchIntVal :: bufferType -> m (Maybe Int)
>   fetchStringVal :: bufferType -> m (Maybe String)
>   fetchDoubleVal :: bufferType -> m (Maybe Double)
>   fetchDatetimeVal :: bufferType -> m (Maybe CalendarTime)


> class DBType a where
>   allocBufferFor ::
>     (Buffer m bufferType) =>
>     a -> Position -> m bufferType
>   fetchCol :: (Buffer m bufferType) => bufferType -> m a


> class QueryIteratee iterType seedType | iterType -> seedType where
>   iterApply ::
>     (MonadIO m, Buffer m bufferType) =>
>     [bufferType] -> seedType -> iterType -> m (IterResult seedType)
>   allocBuffers ::
>     (MonadIO m, Buffer m bufferType) =>
>     iterType -> Position -> m [bufferType]


> instance (DBType a) =>
>   QueryIteratee (a -> seedType -> IterResult seedType) seedType where
>   iterApply [buf] seed fn = do
>     v <- fetchCol buf
>     return (fn v seed)
>   allocBuffers _ n = sequence [allocBufferFor (undefined::a) n]

> instance (QueryIteratee iterType' seedType, DBType a) =>
>   QueryIteratee (a -> iterType') seedType where
>   iterApply (buffer:moreBuffers) seed fn = do
>     v <- fetchCol buffer
>     iterApply moreBuffers seed (fn v)
>   allocBuffers fn n = do
>     buffer <- allocBufferFor (undefined::a) n
>     moreBuffers <- allocBuffers (undefined::iterType') (n+1)
>     return (buffer:moreBuffers)


Used by instances of DBType to throw an exception
when a null (Nothing) is returned.
Will work for any type, as you pass the fetch function in the fetcher arg.

> throwIfDBNull :: (DBType a, Buffer m bufferType) =>
>   bufferType -> (bufferType -> m (Maybe a)) -> m a
> throwIfDBNull buffer fetcher = do
>   v <- fetcher buffer
>   case v of
>     Nothing -> do
>       row <- currentRowNum
>       col <- columnPosition buffer
>       throwDB (DBUnexpectedNull row col)
>     Just m -> return m



--------------------------------------------------------------------
-- A Query monad and cursors.
--------------------------------------------------------------------


> type CollEnumerator iter m seed = iter -> seed -> m seed
> type Self           iter m seed = iter -> seed -> m seed
> type CFoldLeft      iter m seed = Self iter m seed -> CollEnumerator iter m seed

> newtype DBCursor ms a = DBCursor (IORef (a, Maybe (Bool-> ms (DBCursor ms a))))

> class (MonadIO ms) => MonadQuery m ms q | m -> ms, ms -> q, ms q -> m where
>   doQuery ::
>     (QueryIteratee iterType seedType) =>
>     QueryText -> iterType -> seedType -> ms seedType
>   getQuery:: m q
>   makeQuery:: QueryText -> ms q
>   doQuery1Maker::
>     (QueryIteratee iterType seedType) =>
>     QueryText -> iterType -> ms (CFoldLeft iterType ms seedType,ms ())
>   openCursor :: 
>     (QueryIteratee iterType seedType) =>
>     QueryText -> iterType -> seedType -> ms (DBCursor ms seedType)
>   fetch1 :: m Bool


Cursors are automatically closed and freed when:
 - the iteratee returns Left a
 - the collection (fetch) is exhausted.

The only situations in which the user/programmer
must explicitly close the cursor are:
 - when they want to terminate the fetch early
   i.e. before the collection is exhuasted and before the iteratee returns Left
 - when an exception (any sort of exception) is raised. For example:

  c <- openCursor query iter []
  catchReaderT
    ( do
      r <- cursorCurrent c
      ...
      cursorClose c
      return something
    ) ( e\ -> do
      cursorClose c
      liftIO $ throwIO e
    )

or
  c <- openCursor query iter []
  withCursorBracket c $ do
      r <- cursorCurrent c
      ...
      return something


Returns True or False.

> cursorIsEOF :: (MonadIO m) => DBCursor ms a -> m Bool
> cursorIsEOF (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe True (const False) maybeF
>   case maybeF of
>     Nothing -> return True
>     Just f -> return False

cursorCurrent returns the results fetched so far, processed by iteratee function.

> cursorCurrent :: (MonadIO m) => DBCursor ms a -> m a
> cursorCurrent (DBCursor ref) = do
>   (v, _) <- liftIO $ readIORef ref
>   return v

dbCursorNext and dbCursorClose return the latest cursor.

> cursorNext :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorNext (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe (throwDB DBNoData) ($ True) maybeF
>   case maybeF of
>     Nothing -> throwDB DBNoData
>     Just f -> f True

> cursorClose :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorClose c@(DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe (return c) ($ False) maybeF
>   case maybeF of
>     Nothing -> return c
>     Just f -> f False


> withCursorBracket :: (MonadIO (ReaderT r IO)) =>
>   DBCursor (ReaderT r IO) a -> ReaderT r IO b -> ReaderT r IO b
> withCursorBracket cursor action = catchReaderT ( do
>     v <- action
>     _ <- cursorClose cursor
>     return v
>   ) (\e -> do
>     _ <- cursorClose cursor
>     liftIO $ throwIO e
>   )



-- Polymorphic over m.

> runfetch ::
>   ( Monad m
>   , MonadIO (t m)
>   , MonadQuery (t m) ms q
>   , Buffer (t m) bufferType
>   , QueryIteratee iterType seedType
>   , MonadTrans t
>   ) =>
>      t m a
>   -> [bufferType]
>   -> (iterType -> seedType -> m seedType)
>   -> (iterType -> seedType -> t m seedType)

> runfetch finalizers buffers self iteratee seedVal = do
>     let
>       handle seedVal True = do
>         row <- iterApply buffers seedVal iteratee
>         handleIter row
>       handle seedVal False = do
>         finalizers
>         return seedVal
>       handleIter (Right seed) = lift $ self iteratee seed
>       handleIter (Left seed) = do
>         finalizers
>         return seed
>     v <- fetch1
>     handle seedVal v
