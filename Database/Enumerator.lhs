
|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Abstract database interface, providing a left-fold enumerator
and cursor operations.
 
Some functions in this module are not part of the Enumerator interface
e.g. @runfetch@, @iterApply@, @allocBuffers@.
They are in here because they are generic i.e. they do not depend
on any particular DBMS implementation.
 
There is a stub ("Database.Oracle.OCIStub") for the Oracle OCI implementation.
This lets you run the test cases without having a working Oracle installation.
You need to swtich in the @OCIStub@ module, and switch out the @OCIEnumerator@ module.
See "Database.Oracle.Enumerator" for details.
 
Additional reading:
 
 * <http://pobox.com/~oleg/ftp/Haskell/misc.html#fold-stream>
 
 * <http://pobox.com/~oleg/ftp/papers/LL3-collections-enumerators.txt>
 
 * <http://www.eros-os.org/pipermail/e-lang/2004-March/009643.html>


--------- Haddock notes:

 - An "empty line" (as mentioned subsequently in these notes) is actually a single space on a line.
   This results in a "-- " line in the generated .hs file,
   which continues the comment block from Haddock's point of view.
 - The module comment must contain a empty line between Portability and the description.
 - bullet-lists:
     - items must be preceded by an empty line.
     - each list item must start with "*".
 - code-sections:
     - must be preceded by an empty line.
     - use " >" rather than @...@, because "@" allows markup translation, where " >" doesn't.
 - @inline code (monospaced font)@
 - /emphasised text/
 - link to "Another.Module".
 - link to 'SomeType' in same module.
 - link to 'Another.Module.SomeType'.
 - <http:/www.haskell.org/haddock>



> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Enumerator where

> import System.Time (CalendarTime)
> import Data.Dynamic (Typeable)
> import Data.IORef
> import Control.Monad.Trans
> import Control.Exception (catchDyn, throwDyn, throwIO, Exception)
> import qualified Control.Exception (catch)
> import Control.Monad.Reader


|See "Database.Oracle.OCIEnumerator" for instructions on adding new types.
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
>   | Serializable  -- ^ for alternative spellers
>   deriving Show

> type BufferSize = Int
> type BufferHint = (BufferSize, DBColumnType)
> type Position = Int
> type IterResult seedType = Either seedType seedType
> type IterAct m seedType = seedType -> m (IterResult seedType)
> type QueryText = String
> type ColNum = Int
> type RowNum = Int



--------------------------------------------------------------------
-- ** Exceptions and handlers
--------------------------------------------------------------------

If we can't derive Typeable then the following code should do the trick:
 > data DBException = DBError ...
 > dbExceptionTc :: TyCon
 > dbExceptionTc = mkTyCon "Database.Enumerator.DBException"
 > instance Typeable DBException where typeOf _ = mkAppTy dbExceptionTc []

> data DBException
>   -- | DBMS error message.
>   = DBError Int String
>   -- | the iteratee function used for queries accepts both nullable (Maybe) and
>   -- non-nullable types. If the query itself returns a null in a column where a
>   -- non-nullable type was specified, we can't handle it, so DBUnexpectedNull is thrown.
>   | DBUnexpectedNull RowNum ColNum
>   -- | Thrown by cursor functions if you try to fetch after the end.
>   | DBNoData
>   deriving (Typeable, Show)


> catchDB :: IO a -> (DBException -> IO a) -> IO a
> catchDB = catchDyn
> throwDB :: DBException -> a
> throwDB = throwDyn

|This simple handler reports the error to @stdout@ and swallows it
i.e. it doesn't propagate.

> basicDBExceptionReporter :: DBException -> IO ()
> basicDBExceptionReporter (DBError e m) = putStrLn m
> basicDBExceptionReporter (DBUnexpectedNull r c) = putStrLn $ "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
> basicDBExceptionReporter (DBNoData) = putStrLn "Fetch: no more data."

|If you want to trap a specific error number, use this.
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


|These two functions let us catch (and rethrow) exceptions in the ReaderT monad.
We need these because 'Control.Exception.catch' is in the IO monad,
but /not/ MonadIO.

> shakeReaderT :: ((ReaderT r m1 a1 -> m1 a1) -> m a) -> ReaderT r m a
> shakeReaderT f = ReaderT $ \r -> f (\lm -> runReaderT lm r)

> catchReaderT :: ReaderT r IO a -> (Control.Exception.Exception -> ReaderT r IO a) -> ReaderT r IO a
> catchReaderT m h = shakeReaderT $ \sinker -> Control.Exception.catch (sinker m) (sinker . h)


--------------------------------------------------------------------
-- ** Session monad.
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
-- ** Buffers and QueryIteratee.
--------------------------------------------------------------------

|A type class for Buffers.

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


|Define the Iteratee interface as a class.
The implementations (instances) are below,
as they're not DBMS specific.

> class (MonadIO m) => QueryIteratee m iterType seedType |
>                      iterType -> seedType, iterType -> m where
>   iterApply ::
>     (Buffer m bufferType) =>
>     [bufferType] -> seedType -> iterType -> m (IterResult seedType)
>   allocBuffers ::
>     (Buffer m bufferType) =>
>     iterType -> Position -> m [bufferType]

|This instance of the class is the terminating case
i.e. where the iteratee function has one argument left.
The argument is applied, and the result returned.

> instance (DBType a, MonadIO m) =>
>   QueryIteratee m (a -> seedType -> m (IterResult seedType)) seedType where
>   iterApply [buf] seed fn = do
>     v <- fetchCol buf
>     fn v seed
>   allocBuffers _ n = sequence [allocBufferFor (undefined::a) n]


|This instance of the class implements the initial and continuation cases.

> instance (QueryIteratee m iterType' seedType, DBType a) =>
>   QueryIteratee m (a -> iterType') seedType where
>   iterApply (buffer:moreBuffers) seed fn = do
>     v <- fetchCol buffer
>     iterApply moreBuffers seed (fn v)
>   allocBuffers fn n = do
>     buffer <- allocBufferFor (undefined::a) n
>     moreBuffers <- allocBuffers (undefined::iterType') (n+1)
>     return (buffer:moreBuffers)



--------------------------------------------------------------------
-- ** A Query monad and cursors.
--------------------------------------------------------------------


> type CollEnumerator iter m seed = iter -> seed -> m seed
> type Self           iter m seed = iter -> seed -> m seed
> type CFoldLeft      iter m seed = Self iter m seed -> CollEnumerator iter m seed

> newtype DBCursor ms a = DBCursor (IORef (a, Maybe (Bool-> ms (DBCursor ms a))))

> class (MonadIO ms) => MonadQuery m ms q | m -> ms, ms -> q, ms q -> m where
>   doQuery ::
>     (QueryIteratee m iterType seedType) =>
>     QueryText -> iterType -> seedType -> ms seedType
>   getQuery:: m q
>   makeQuery:: QueryText -> ms q
>   doQuery1Maker::
>     (QueryIteratee m iterType seedType) =>
>     QueryText -> iterType -> ms (CFoldLeft iterType ms seedType,ms ())
>   openCursor :: 
>     (QueryIteratee m iterType seedType) =>
>     QueryText -> iterType -> seedType -> ms (DBCursor ms seedType)
>   fetch1 :: m Bool


|Returns True or False. Cursors are automatically closed and freed when:
 
 * the iteratee returns Left a
 
 * the collection (fetch) is exhausted.

|The only situations in which the user-programmer
must explicitly close the cursor are:
 
 * when they want to terminate the fetch early
   i.e. before the collection is exhuasted and before the iteratee returns Left
 
 * when an exception (any sort of exception) is raised. For example:
 
 >  c <- openCursor query iter []
 >  catchReaderT
 >    ( do
 >      r <- cursorCurrent c
 >      ...
 >      cursorClose c
 >      return something
 >    ) ( e\ -> do
 >      cursorClose c
 >      liftIO $ throwIO e
 >    )

|To make life easier, we've created a withCursorBracket function:

 >  withCursorBracket query1 iter1 [] $ \c1 -> do
 >    withCursorBracket query2 iter2 [] $ \c2 -> do
 >      r1 <- cursorCurrent c1
 >      r2 <- cursorCurrent c2
 >      ...
 >      return something
 

> cursorIsEOF :: (MonadIO m) => DBCursor ms a -> m Bool
> cursorIsEOF (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe True (const False) maybeF
>   case maybeF of
>     Nothing -> return True
>     Just f -> return False

|Returns the results fetched so far, processed by iteratee function.

> cursorCurrent :: (MonadIO m) => DBCursor ms a -> m a
> cursorCurrent (DBCursor ref) = do
>   (v, _) <- liftIO $ readIORef ref
>   return v

|Returns the latest cursor.

> cursorNext :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorNext (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe (throwDB DBNoData) ($ True) maybeF
>   case maybeF of
>     Nothing -> throwDB DBNoData
>     Just f -> f True

|Returns the latest cursor.

> cursorClose :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorClose c@(DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe (return c) ($ False) maybeF
>   case maybeF of
>     Nothing -> return c
>     Just f -> f False


| Ensures cursor resource is properly tidied up in exceptional cases.
Propagates exceptions after closing cursor.

> withCursorBracket :: (MonadQuery m (ReaderT r IO) q, QueryIteratee m iterType seedType	) =>
>   QueryText  -- ^ query string
>   -> iterType  -- ^ iteratee function
>   -> seedType  -- ^ seed value
>   -> (DBCursor (ReaderT r IO) seedType -> ReaderT r IO a)  -- ^ action that takes a cursor
>   -> ReaderT r IO a
> withCursorBracket query iteratee seed action = do
>   cursor <- openCursor query iteratee seed
>   catchReaderT ( do
>       v <- action cursor
>       _ <- cursorClose cursor
>       return v
>     ) (\e -> do
>       _ <- cursorClose cursor
>       liftIO $ throwIO e
>     )


--------------------------------------------------------------------
-- ** Misc.
--------------------------------------------------------------------

|Polymorphic over m.
Not needed by programmer but moved into this module because it doesn't
depend on any DBMS implementation details.

> runfetch ::
>   ( Monad m
>   , MonadIO (t m)
>   , MonadQuery (t m) ms q
>   , Buffer (t m) bufferType
>   , QueryIteratee (t m) iterType seedType
>   , MonadTrans t
>   ) =>
>      (forall a. t m a -> m a) -- ^ into query
>   ->   t m a  -- ^ finaliser
>   -> [bufferType]  -- ^ list of buffers
>   -> (iterType -> seedType -> m seedType)  -- ^ self i.e. function taking iteratee and seed, returning same type as seed
>   -> iterType  -- ^ iteratee
>   -> seedType  -- ^ seed value
>   -> m seedType  -- ^ return value same type as seed

> runfetch down finalizers buffers self iteratee seedVal = do
>   let
>     handle seedVal True = (down (iterApply buffers seedVal iteratee)) >>= handleIter
>     handle seedVal False = (down finalizers) >> return seedVal
>     handleIter (Right seed) = self iteratee seed
>     handleIter (Left seed) = (down finalizers) >> return seed
>   (down fetch1) >>= handle seedVal


|Used by instances of DBType to throw an exception
when a null (Nothing) is returned.
Will work for any type, as you pass the fetch action in the fetcher arg.

> throwIfDBNull :: (DBType a, Buffer m bufferType) =>
>   bufferType  -- ^ Buffer.
>   -> (bufferType -> m (Maybe a))  -- ^ Action to get (fetch) value from buffer; this is applied to buffer.
>   -> m a  -- ^ If the value in the buffer is not null, it is returned.
> throwIfDBNull buffer fetcher = do
>   v <- fetcher buffer
>   case v of
>     Nothing -> do
>       row <- currentRowNum
>       col <- columnPosition buffer
>       throwDB (DBUnexpectedNull row col)
>     Just m -> return m


|Useful utility function, for SQL weenies.

> ifNull :: Maybe a  -- ^ Nullable value
>   -> a  -- ^ value to substitute if parm-1 null
>   -> a
> ifNull value subst = maybe subst id value



| Another useful utility function.
Use this to return a value from an iteratee function (the one passed to doQuery).
Note that you should probably nearly always use the strict version.

> result :: (Monad m) => IterAct m a
> result x = return (Right x)


|A strict version. This is recommended unless you have a specific need for laziness,
as the lazy version will gobble stack and heap.
If you have a large result-set (in the order of 10-100K rows or more),
it will exhaust the standard 1M GHC stack.

> result' :: (Monad m) => IterAct m a
> result' x = return (Right $! x)
