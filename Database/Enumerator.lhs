
|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Abstract database interface, providing a left-fold enumerator
and cursor operations.
 
Some functions in this module are not strictly part of the Enumerator interface
e.g. @iterApply@, @allocBuffers@.
They are in here because they are generic i.e. they do not depend
on any particular DBMS implementation.
They are a part of the middle layer - enumerator -
which is database-independent.
 
There is a stub: "Database.Stub.Enumerator".
This lets you run the test cases without having a working DBMS installation.
 
Additional reading:
 
 * <http://pobox.com/~oleg/ftp/Haskell/misc.html#fold-stream>
 
 * <http://pobox.com/~oleg/ftp/papers/LL3-collections-enumerators.txt>
 
 * <http://www.eros-os.org/pipermail/e-lang/2004-March/009643.html>


> {-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

> module Database.Enumerator
>   (
>     -- * Usage
>
>     -- $usage_example
>
>     -- ** Iteratee Functions
>
>     -- $usage_iteratee
>
>     -- ** result and result'
>
>     -- $usage_result
>
>       DBColumnType(..), IsolationLevel(..)
>     , BufferSize, BufferHint, Position
>     , IterResult, IterAct, QueryText, ColNum, RowNum
>
>     -- * Exceptions and handlers
>     , DBException(..)
>     , catchDB, throwDB, basicDBExceptionReporter, catchDBError, ignoreDBError
>     , shakeReaderT, catchReaderT
>
>     -- * Session monad.
>     , MonadSession(..)
>
>     -- * Buffers.
>     --   These are not directly used by the user: they merely provide the
>     --   interface between the low and the middle layers of Takusen.
>     , DBType(..), QueryIteratee(..)
>
>     -- * A Query monad and cursors.
>     , DBCursor(..),  QueryResourceUsage(..)
>     , MonadQuery(..)
>     , defaultResourceUsage
>     , doQuery, doQueryBind
>     , doQueryTuned
>     , withCursor, withCursorBind
>     , withCursorTuned
>     , withTransaction
>     , cursorIsEOF, cursorCurrent, cursorNext, cursorClose
>
>     -- * Misc.
>     , throwIfDBNull, ifNull, result, result'
>     , liftIO, throwIO
>   ) where

> import System.Time (CalendarTime)
> import Data.Dynamic (Typeable)
> import Data.IORef
> import Control.Monad.Trans
> import Control.Exception (catchDyn, throwDyn, throwIO, Exception)
> import qualified Control.Exception (catch)
> import Control.Monad.Reader


|If you add a new type here, you must add an 'DBType' instance for it
to each of the implementation modules
e.g. "Database.Oracle.OCIEnumerator"
, "Database.Sqlite.SqliteEnumerator"

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

> type SessionQuery s a = ReaderT s IO a


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
> basicDBExceptionReporter (DBError e m) = putStrLn $ (show e) ++ ": " ++ m
> basicDBExceptionReporter (DBUnexpectedNull r c) =
>   putStrLn $ "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
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


|'shakeReaderT' and 'catchReaderT' let us catch (and rethrow) exceptions in the ReaderT monad.
We need these because 'Control.Exception.catch' is in the IO monad, but /not/ MonadIO.

> shakeReaderT :: ((ReaderT r m1 a1 -> m1 a1) -> m a) -> ReaderT r m a
> shakeReaderT f = ReaderT $ \r -> f (\lm -> runReaderT lm r)

> catchReaderT :: ReaderT r IO a -> (Control.Exception.Exception -> ReaderT r IO a) -> ReaderT r IO a
> catchReaderT m h = shakeReaderT $ \sinker -> Control.Exception.catch (sinker m) (sinker . h)


--------------------------------------------------------------------
-- ** Session monad
--------------------------------------------------------------------

|The MonadSession class describes a database session to a particular
DBMS.  Oracle has its own Session object, SQLite has its own
session object (which maintains the connection handle to the database
engine and other related stuff). Session objects for different databases
normally have different types -- yet they all belong to the class MonadSession
so we can do generic operations like `commit', `executeDDL', etc. 
in a database-independent manner.
 
Session objects per se are created by database connection\/login functions.
 
The class MonadSession is thus an interface between low-level (and
database-specific) code and the high-level, database-independent code.
The methods of the class MonadSession are visible (and highly useful)
to the user. They are also used internally.

> class (MonadIO mb) => MonadSession m mb s | m -> mb, m -> s, mb s -> m where
>   runSession :: s -> m a -> mb a
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


> -- This might be useful
> type MSess s = ReaderT s IO



--------------------------------------------------------------------
-- ** Buffers and QueryIteratee
--------------------------------------------------------------------

|A \'buffer\' means a column buffer: a data structure that points to a
block of memory allocated for the values of one particular
column. Since a query normally fetches a row of several columns, we
typically deal with a list of column buffers. Although the column data
are typed (e.g., Integer, CalendarDate, etc), column buffers hide that
type. Think of the column buffer as Dynamics. The class DBType below
describes marshalling functions, to fetch a typed value out of the
`untyped' columnBuffer.
 
Different DBMS's (that is, different session objects) have, in
general, columnBuffers of different types: the type of Column Buffer
is specific to a database.
So, MonadSession (m) uniquely determines the buffer type (b).
 
The class DBType is not used by the end-user.
It is used to tie up low-level database access and the enumerator.
A database-specific library must provide a set of instances for DBType.

> class DBType a m b | m -> b where
>   allocBufferFor :: a -> Position -> m b
>   fetchCol :: b -> m a
>   bindPos :: Position -> a -> m ()


|The class QueryIteratee is not for the end user. It provides the
interface between the low- and the middle-layers of Takusen. The
middle-layer -- enumerator -- is database-independent then.

> class (MonadIO m) => QueryIteratee m i s b
>     | i -> s, i -> m, m -> b where
>   iterApply :: [b] -> s -> i -> m (IterResult s)
>   allocBuffers :: i -> Position -> m [b]

|This instance of the class is the terminating case
i.e. where the iteratee function has one argument left.
The argument is applied, and the result returned.

> instance (DBType a m b, MonadIO m) =>
>   QueryIteratee m (a -> s -> m (IterResult s)) s b where
>   iterApply [buf] seed fn = do
>     v <- fetchCol buf
>     fn v seed
>   allocBuffers _ n = sequence [allocBufferFor (undefined::a) n]


|This instance of the class implements the starting and continuation cases.

> instance (QueryIteratee m i' s b, DBType a m b)
>     => QueryIteratee m (a -> i') s b where
>   iterApply (buffer:moreBuffers) seed fn = do
>     v <- fetchCol buffer
>     iterApply moreBuffers seed (fn v)
>   allocBuffers fn n = do
>     buffer <- allocBufferFor (undefined::a) n
>     moreBuffers <- allocBuffers (undefined::i') (n+1)
>     return (buffer:moreBuffers)



--------------------------------------------------------------------
-- ** A Query monad and cursors
--------------------------------------------------------------------

|At present the only resource tuning we support is the number of rows
prefetched by the FFI library.
We use a record to (hopefully) make it easy to add other tuning parameters later.

> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 1



> type CollEnumerator i m s = i -> s -> m s
> type Self           i m s = i -> s -> m s
> type CFoldLeft      i m s = Self i m s -> CollEnumerator i m s

|A DBCursor is an IORef-mutable-pair @(a, Maybe f)@, where @a@ is the result-set so far,
and @f@ is a function that fetches and returns the next row (when applied to True),
or closes the cursor (when applied to False).
If @Maybe@ f is @Nothing@, then the result-set has been exhausted
(or the iteratee function terminated early),
and the cursor has already been closed.
 
> newtype DBCursor ms a = DBCursor (IORef (a, Maybe (Bool-> ms (DBCursor ms a))))


|The class MonadQuery describes the class of query objects. Each
database (that is, each Session object) has its own Query object. The
class MonadQuery implements the database-independent interface to
query object. The class provides the interface between low-level layer
and the middle layer (enumerators) of Takusen. An implementor of a
database-specific layer must provide an instance of MonadQuery.
The class MonadQuery is not visible and usable by the end-user.

> class (MonadIO ms) => MonadQuery m ms q b
>     | m -> ms, ms -> q, ms -> m, ms -> b
>   where
>   getQuery :: m q
>   makeQuery :: QueryText -> QueryResourceUsage -> ms q
>   runQuery :: m a -> q -> ms a
>   destroyQuery :: q -> ms () -- after buffers are freed, close the STMT
>   fetch1Row :: m Bool  -- fetch one row
>   allocBuffer :: BufferHint -> Position -> m b
>   columnPosition :: b -> m Int
>   currentRowNum :: m Int
>   freeBuffer :: bufferType -> m ()


The following functions provide the high-level query interface,
for the end user.

|The simplest doQuery interface.
No bind action, default resource usage.
Resource usage settings are implementation dependent.
(For the Oracle OCI, we only cache one row.)

> doQuery ::
>   ( MonadQuery m (ReaderT r IO) q b
>   , MonadSession (ReaderT r IO) mb r
>   , QueryIteratee m i s b
>   , DBType Int m b
>   ) => QueryText -> i -> s -> ReaderT r IO s
>
> -- We must give the empty list a concrete type, or rather,
> -- a type that is an instance of DBType.
> -- It doesn't matter what type it has, as the list is empty
> -- (it just contains bind values, and here there are none).
> -- If we don't give it a type, then the type checker will
> -- be unable (in user code) to resolve the empty list
> -- to one of the DBType instances, which it must be.
> doQuery sql iteratee seed = doQueryBind sql ([]::[Int]) iteratee seed


|Adds bind action.

> doQueryBind ::
>   ( MonadQuery m (ReaderT r IO) q b
>   , MonadSession (ReaderT r IO) mb r
>   , QueryIteratee m i s b
>   , DBType a m b
>   ) => QueryText -> [a] -> i -> s -> ReaderT r IO s
>
> doQueryBind sql = doQueryTuned defaultResourceUsage sql



|Version of doQuery which gives you a bit more control over how 
rows are fetched.
At the moment we only support specifying the numbers of rows prefetched 
(cached).
doQuery is essentially the result of applying lfold_nonrec_to_rec
to doQueryMaker (which it not exposed by the module).

> doQueryTuned ::
>   ( MonadQuery m (ReaderT r IO) q b
>   , MonadSession (ReaderT r IO) mb r
>   , QueryIteratee m i s b
>   , DBType a m b
>   ) => QueryResourceUsage -> QueryText -> [a] -> i -> s -> ReaderT r IO s
>
> doQueryTuned resourceUsage sqltext bindvals iteratee seed = do
>     (lFoldLeft, finalizer) <- doQueryMaker sqltext bindvals iteratee resourceUsage
>     catchReaderT (fix lFoldLeft iteratee seed)
>       (\e -> do
>         finalizer
>         liftIO $ throwIO e
>       )

|This is the auxiliary function.

> doQueryMaker ::
>   ( MonadSession ms mb r
>   , MonadQuery m ms q b
>   , QueryIteratee m i s b
>   , DBType a m b
>   ) => QueryText -> [a] -> i -> QueryResourceUsage ->
>        ms ((i -> s -> ms s) -> i -> s -> ms s, ms ())
> doQueryMaker sqltext bindvals iteratee resourceUsage = do
>     sess <- getSession
>     query <- makeQuery sqltext resourceUsage
>     let bindAction = sequence_ $ map (\(p, v) -> bindPos p v) (zip [1..] bindvals)
>     runQuery bindAction query
>     let inQuery m = runQuery m query
>     buffers <- inQuery $ allocBuffers iteratee 1
>     let
>       finaliser = do
>         inQuery $ mapM_ freeBuffer buffers
>         destroyQuery query
>       hFoldLeft self iteratee initialSeed = do
>         let
>           handle seed True = (inQuery (iterApply buffers seed iteratee)) 
>             >>= handleIter
>           handle seed False = (finaliser) >> return seed
>           handleIter (Right seed) = self iteratee seed
>           handleIter (Left seed) = (finaliser) >> return seed
>         (inQuery fetch1Row) >>= handle initialSeed
>     return (hFoldLeft, finaliser)



Cursor stuff. First, an auxiliary function, not seen by the user.

> openCursorTuned resourceUsage sqltext bindvals iteratee seed = do
>     ref <- liftIO$ newIORef (seed,Nothing)
>     (lFoldLeft, finalizer) <- doQueryMaker sqltext bindvals iteratee resourceUsage
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



|cursorIsEOF's return value tells you if there are any more rows or not.
If you call 'cursorNext' when there are no more rows,
a 'DBNoData' exception is thrown.
Cursors are automatically closed and freed when:
 
 * the iteratee returns @Left a@
 
 * the query result-set is exhausted.

|To make life easier, we've created a 'withCursor' function,
which will clean up if an error (exception) occurs,
or the code exits early.
You can nest them to get interleaving, if you desire:
 
 >  withCursor query1 iter1 [] $ \c1 -> do
 >    withCursor query2 iter2 [] $ \c2 -> do
 >      r1 <- cursorCurrent c1
 >      r2 <- cursorCurrent c2
 >      ...
 >      return something
 

> cursorIsEOF :: (MonadIO m) => DBCursor ms a -> m Bool
> cursorIsEOF (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   return $ maybe True (const False) maybeF

|Returns the results fetched so far, processed by iteratee function.

> cursorCurrent :: (MonadIO m) => DBCursor ms a -> m a
> cursorCurrent (DBCursor ref) = do
>   (v, _) <- liftIO $ readIORef ref
>   return v

|Advance the cursor. Returns the cursor. The return value is usually ignored.

> cursorNext :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorNext (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   maybe (throwDB DBNoData) ($ True) maybeF

|Returns the cursor. The return value is usually ignored.

> cursorClose :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorClose c@(DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   maybe (return c) ($ False) maybeF


|Ensures cursor resource is properly tidied up in exceptional cases.
Propagates exceptions after closing cursor.

> withCursor ::
>   ( MonadQuery m (ReaderT r IO) q b
>   , MonadSession (ReaderT r IO) mb r
>   , QueryIteratee m i s b
>   , DBType Int m b
>  ) =>
>      QueryText  -- ^ query string
>   -> i  -- ^ iteratee function
>   -> s  -- ^ seed value
>   -> (DBCursor (ReaderT r IO) s -> ReaderT r IO c)  -- ^ action that takes a cursor
>   -> ReaderT r IO c
>
> withCursor sqltext = withCursorBind sqltext ([]::[Int])


> withCursorBind ::
>   ( MonadQuery m (ReaderT r IO) q b
>   , MonadSession (ReaderT r IO) mb r
>   , QueryIteratee m i s b
>   , DBType a m b
>  ) =>
>      QueryText  -- ^ query string
>   -> [a]  -- ^ bind values
>   -> i  -- ^ iteratee function
>   -> s  -- ^ seed value
>   -> (DBCursor (ReaderT r IO) s -> ReaderT r IO c)  -- ^ action that takes a cursor
>   -> ReaderT r IO c
>
> withCursorBind sqltext = withCursorTuned defaultResourceUsage sqltext


|Version which lets you tune resource usage.

> withCursorTuned ::
>   ( MonadQuery m (ReaderT r IO) q b
>   , MonadSession (ReaderT r IO) mb r
>   , QueryIteratee m i s b
>   , DBType a m b
>   ) =>
>      QueryResourceUsage  -- ^ resource usage tuning
>   -> QueryText  -- ^ query string
>   -> [a]  -- ^ bind values
>   -> i  -- ^ iteratee function
>   -> s  -- ^ seed value
>   -> (DBCursor (ReaderT r IO) s -> ReaderT r IO c)  -- ^ action that takes a cursor
>   -> ReaderT r IO c
>
> withCursorTuned resourceUsage query bindvals iteratee seed action = do
>   cursor <- openCursorTuned resourceUsage query bindvals iteratee seed
>   catchReaderT ( do
>       v <- action cursor
>       _ <- cursorClose cursor
>       return v
>     ) (\e -> do
>       _ <- cursorClose cursor
>       liftIO $ throwIO e
>     )


|Perform an action as a transaction: commit afterwards,
unless there was an exception, in which case rollback.

> withTransaction ::
>   ( MonadSession (ReaderT r IO) mb r
>   , MonadIO (ReaderT r IO)
>   ) =>
>   IsolationLevel -> ReaderT r IO a -> ReaderT r IO a
>
> withTransaction isolation action = do
>     commit
>     beginTransaction isolation
>     catchReaderT ( do
>         v <- action
>         commit
>         return v
>       ) (\e -> do
>         rollback
>         liftIO $ throwIO e
>       )


--------------------------------------------------------------------
-- ** Misc.
--------------------------------------------------------------------

|Used by instances of DBType to throw an exception
when a null (Nothing) is returned.
Will work for any type, as you pass the fetch action in the fetcher arg.

> throwIfDBNull :: (Monad m, MonadQuery m ms q b, DBType a m b) =>
>   b  -- ^ Buffer.
>   -> (b -> m (Maybe a))  -- ^ Action to get (fetch) value from buffer; this is applied to buffer.
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

> ifNull :: Maybe a  -- ^ nullable value
>   -> a  -- ^ value to substitute if first parameter is null
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
it is likely to exhaust the standard 1M GHC stack.
Whether or not 'result' eats memory depends on what @x@ does:
if it's a delayed computation then it almost certainly will.
This includes consing elements onto a list,
and arithmetic operations (counting, summing, etc).

> result' :: (Monad m) => IterAct m a
> result' x = return (Right $! x)












--------------------------------------------------------------------
-- Usage notes
--------------------------------------------------------------------


-- $usage_example
 
You should always import this module (which contains all of the useful
functions), and the DBMS-specific modules for the DBMS's you need to use.
The DBMS-specific modules export only connect + disconnect + Session,
where Session is a DBMS-specific type.
 
 > -- sample code, doesn't necessarily compile
 > module MyDbExample is
 >
 > import Database.Enumerator
 > import Database.Oracle.Enumerator
 >
 > query1Iteratee :: (Monad m) => Int -> String -> Double -> IterAct m [(Int, String, Double)]
 > query1Iteratee a b c accum = result' $ (a, b, c):accum
 >
 > -- simple query, returning reversed list of rows.
 > query1 = doQuery "select a, b, c from x" query1Iteratee []
 >
 > -- non-query actions. Use runSession to execute.
 > otherActions :: Session -> IO ()
 > otherActions session = do
 >   runSession session ( do
 >       executeDDL "create table blah"
 >       executeDML "insert into blah ..."
 >       commit
 >     )
 >
 > main :: IO ()
 > main = do
 >   session <- connect "user" "password" "server"
 >   -- use runSession to execute query (it has type SessionQuery)
 >   r <- runSession session query1
 >   putStrLn $ show r
 >   otherActions session
 >   disconnect session
 
@Session@ is an opaque type returned by the connect function.
@connect@ is only exported by the DBMS-specific module,
not from this module ('Database.Enumerator').
@Session@ is an instance of 'MonadSession',
and to use the 'MonadSession' functions you invoke them via runSession.
 

-- $usage_iteratee
 
@doQuery@ takes an iteratee function, of n arguments.
Argument n is the accumulator (or seed).
For each row that is returned by the query,
the iteratee function is called with the data from that row in
arguments 1 to n-1, and the current accumulated value in the argument n.
 
The iteratee function returns the next value of the accumulator,
wrapped in an 'Data.Either.Either'.
If the 'Data.Either.Either' value is @Left@, then the query will terminate,
with the new value of the accumulator\/seed returned.
If the value is @Right@, then the query will continue, with the next row
begin fed to the iteratee function, along with the new accumulator\/seed.
 
In the example above, @query1Iteratee@ simply conses the new row (as a tuple)
to the front of the accumulator. The initial seed passed to @doQuery@ was an empty list.
Consing the rows to the front of the list results in a list that is the result set
with the rows in reverse order.
 
The iteratee function exists in the 'MonadQuery' monad,
so if you want to do IO in it you must use 'Control.Monad.Trans.liftIO'
(e.g. @liftIO $ putStrLn \"boo\"@ ) to lift the IO action into 'MonadQuery'.
 
The iteratee function is not restricted to just constructing lists.
For example, a simple counter function would ignore its arguments,
and the accumulator would simply be the count e.g.
 
 > counterIteratee :: (Monad m) => Int -> IterAct m Int
 > counterIteratee _ i = result' $ (1 + i)
 
The iteratee function that you pass to @doQuery@ needs type information,
at least for the arguments if not the return type (which is typically
determined by the type of the seed).
There is a type synonym @IterAct@, which gives some convenience
in writing type signatures for iteratee functions.


-- $usage_result
 
The 'result' (lazy) and 'result\'' (strict) functions are another convenient shorthand
for returning values from iteratee functions. The return type from an interatee is actually
@Either seed seed@, where you return @Right@ if you want processing to continue,
or @Left@ if you want processing to stop before the result-set is exhausted.
The common case is:
 
 > query1Iteratee a b c accum = return (Right ((a, b, c):accum))
 
which we can write as
 
 > query1Iteratee a b c accum = result $ (a, b, c):accum)
 
We have lazy and strict versions of @result@. The strict version is almost certainly
the one you want to use. If you come across a case where the lazy function is useful,
please tell us about it. The lazy function tends to exhaust the stack for large result-sets,
whereas the strict function does not.
This is due to the accumulation of a large number of unevaluated thunks,
and will happen even for simple arithmetic operations such as counting or summing.
 
If you use the lazy function and you have stack\/memory problems, do some profiling.
With GHC:
 
 * ensure the iteratee is a top-level function so that it has its own cost-centre
 
 * compile with @-prof -auto-all@
 
 * run with @+RTS -p -hr -RTS@
 
 * run @hp2ps@ over the resulting @.hp@ file to get a @.ps@ document, and take a look at it.
   Retainer sets are listed on the RHS, and are prefixed with numbers e.g. (13)CAF, (2)SYSTEM.
   At the bottom of the @.prof@ file you'll find the full descriptions of the retainer sets.
   Match the number in parentheses on the @.ps@ graph with a SET in the @.prof@ file;
   the one at the top of the @.ps@ graph is the one using the most memory.
 
You'll probably find that the lazy iteratee is consuming all of the stack with lazy thunks,
which is why we recommend the strict function.




--------------------------------------------------------------------
-- Haddock notes:
--------------------------------------------------------------------

The best way (that I've found) to get a decent introductory/explanatory
section for the module is to break the explanation into named chunks,
put the named chunks at the end, and reference them in the export list.

You can write the introduction inline, as part of the module description,
but Haddock has no way to make headings. If you make an explicit export-list,
you can use the "-- *", "-- **", etc, syntax to give section headings.

(Note: if you don't use an explicit export list, then Haddock will use "-- *" etc
comments to make headings. The headings will appear in the docs in the the locations
as they do in the source, as do functions, data types, etc.)

 - An "empty line" (as mentioned subsequently in these notes) is actually a single space on a line.
   This results in a "-- " line in the generated .hs file,
   which continues the comment block from Haddock's point of view.
 - The module comment must contain a empty line between "Portability: ..." and the description.
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
