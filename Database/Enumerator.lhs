
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
e.g. @runfetch@, @iterApply@, @allocBuffers@.
They are in here because they are generic i.e. they do not depend
on any particular DBMS implementation.
 
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
>     -- Do we need to export these here? They're just class function sigs.
>     -- If we don't then Haddock moans.
>     --, runSession, getSession, beginTransaction, commit, rollback, executeDML, executeDDL
>
>     -- * Buffers and QueryIteratee.
>     , DBType(..), QueryIteratee(..)
>
>     -- * A Query monad and cursors.
>     , DBCursor(..), MonadQuery(..), QueryResourceUsage(..)
>     -- Again, do we need to export class function sigs, when we already export the class?
>     --, doQuery, doQueryTuned, openCursor, setPrefetchRowCount
>     --, getQuery, makeQuery, doQuery1Maker, fetch1
>     , defaultResourceUsage
>     , cursorIsEOF, cursorCurrent, cursorNext, cursorClose
>     , withCursorBracket, withCursorBracketTuned
>     , withTransaction
>
>     -- * Misc.
>     , runfetch, throwIfDBNull, ifNull, result, result'
>   ) where

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
-- ** Buffers and QueryIteratee
--------------------------------------------------------------------

|A class of data-types that can be marshalled to-and-from the DBMS.
This is parameterised over the buffer type too.

> class DBType a m bufferType | m -> bufferType where
>   allocBufferFor :: a -> Position -> m bufferType
>   fetchCol :: bufferType -> m a


|Define the Iteratee interface as a class.
The implementations (instances) are below,
as they're not DBMS specific.

> class (MonadIO m) => QueryIteratee m iterType seedType bufferType |
>                      iterType -> seedType, iterType -> m,
>                      m -> bufferType where
>   iterApply ::
>     [bufferType] -> seedType -> iterType -> m (IterResult seedType)
>   allocBuffers :: iterType -> Position -> m [bufferType]

|This instance of the class is the terminating case
i.e. where the iteratee function has one argument left.
The argument is applied, and the result returned.

> instance (DBType a m bufferType, MonadIO m) =>
>   QueryIteratee m (a -> seedType -> m (IterResult seedType))
>                 seedType bufferType where
>   iterApply [buf] seed fn = do
>     v <- fetchCol buf
>     fn v seed
>   allocBuffers _ n = sequence [allocBufferFor (undefined::a) n]


|This instance of the class implements the starting and continuation cases.

> instance (QueryIteratee m iterType' seedType bufferType, 
>           DBType a m bufferType)
>     => QueryIteratee m (a -> iterType') seedType bufferType where
>   iterApply (buffer:moreBuffers) seed fn = do
>     v <- fetchCol buffer
>     iterApply moreBuffers seed (fn v)
>   allocBuffers fn n = do
>     buffer <- allocBufferFor (undefined::a) n
>     moreBuffers <- allocBuffers (undefined::iterType') (n+1)
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



> type CollEnumerator iter m seed = iter -> seed -> m seed
> type Self           iter m seed = iter -> seed -> m seed
> type CFoldLeft      iter m seed = Self iter m seed -> CollEnumerator iter m seed

|A DBCursor is an IORef-mutable-pair @(a, Maybe f)@, where @a@ is the result-set so far,
and @f@ is a function that fetches and returns the next row (when applied to True),
or closes the cursor (when applied to False).
If @Maybe@ f is @Nothing@, then the result-set has been exhausted
(or the iteratee function terminated early),
and the cursor has already been closed.
 
> newtype DBCursor ms a = DBCursor (IORef (a, Maybe (Bool-> ms (DBCursor ms a))))

> class (MonadIO ms) => MonadQuery m ms q bufferType | 
>                                  m -> ms, ms -> q, ms q -> m, m -> bufferType
>   where
>   -- |Uses default resource usage settings, which are implementation dependent.
>   -- (For the Oracle OCI, we only cache one row.)
>   doQuery ::
>     (QueryIteratee m iterType seedType bufferType) =>
>     QueryText -> iterType -> seedType -> ms seedType
>   -- |Version of doQuery which gives you a bit more control over how rows are fetched.
>   -- At the moment we only support specifying the numbers of rows prefetched (cached).
>   doQueryTuned ::
>     (QueryIteratee m iterType seedType bufferType) =>
>     QueryResourceUsage -> QueryText -> iterType -> seedType -> ms seedType
>   -- |Open a cursor with default resource usage settings.
>   openCursor :: 
>     (QueryIteratee m iterType seedType bufferType) =>
>     QueryText -> iterType -> seedType -> ms (DBCursor ms seedType)
>   -- |Version which lets you tune resource usage.
>   openCursorTuned :: 
>     (QueryIteratee m iterType seedType bufferType) =>
>     QueryResourceUsage -> QueryText -> iterType -> seedType -> ms (DBCursor ms seedType)
>   -- the following aren't really part of the interface the user employs
>   getQuery:: m q
>   makeQuery:: QueryText -> QueryResourceUsage -> ms q
>   doQuery1Maker::
>     (QueryIteratee m iterType seedType bufferType) =>
>     QueryText -> iterType -> QueryResourceUsage -> ms (CFoldLeft iterType ms seedType,ms ())
>   fetch1 :: m Bool
>   allocBuffer :: BufferHint -> Position -> m bufferType
>   columnPosition :: bufferType -> m Int
>   currentRowNum :: m Int
>   freeBuffer :: bufferType -> m ()


|Returns True or False. Cursors are automatically closed and freed when:
 
 * the iteratee returns @Left a@
 
 * the query result-set is exhausted.

|The only situations in which the user-programmer
must explicitly close the cursor are:
 
 * when they want to terminate the fetch early
   i.e. before the collection is exhausted and before the iteratee returns @Left@
 
 * when an exception (any sort of exception) is raised. For example:
 
 >  c <- openCursor query iteratee []
 >  catchReaderT
 >    ( do
 >      <do stuff with the cursor ...>
 >      _ <- cursorClose c
 >      return something
 >    ) ( e\ -> do
 >      _ <- cursorClose c
 >      liftIO $ throwIO e
 >    )

|To make life easier, we've created a 'withCursorBracket' function,
which is exactly the code above.
You can nest them to get the interleaving you desire:
 
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

|Advance the cursor. Returns the cursor.

> cursorNext :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorNext (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe (throwDB DBNoData) ($ True) maybeF
>   case maybeF of
>     Nothing -> throwDB DBNoData
>     Just f -> f True

|Returns the cursor.

> cursorClose :: (MonadIO ms) => DBCursor ms a -> ms (DBCursor ms a)
> cursorClose c@(DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   --maybe (return c) ($ False) maybeF
>   case maybeF of
>     Nothing -> return c
>     Just f -> f False


| Ensures cursor resource is properly tidied up in exceptional cases.
Propagates exceptions after closing cursor.

> withCursorBracket :: (MonadQuery m (ReaderT r IO) q bufferType,
>                       QueryIteratee m iterType seedType bufferType) =>
>   QueryText  -- ^ query string
>   -> iterType  -- ^ iteratee function
>   -> seedType  -- ^ seed value
>   -> (DBCursor (ReaderT r IO) seedType -> ReaderT r IO a)  -- ^ action that takes a cursor
>   -> ReaderT r IO a
> withCursorBracket = withCursorBracketTuned defaultResourceUsage


| Version which lets you tune resource usage.

> withCursorBracketTuned :: (MonadQuery m (ReaderT r IO) q bufferType,
>		             QueryIteratee m iterType seedType bufferType) =>
>   QueryResourceUsage  -- ^ resource usage tuning
>   -> QueryText  -- ^ query string
>   -> iterType  -- ^ iteratee function
>   -> seedType  -- ^ seed value
>   -> (DBCursor (ReaderT r IO) seedType -> ReaderT r IO a)  -- ^ action that takes a cursor
>   -> ReaderT r IO a
> withCursorBracketTuned query iteratee seed resourceUsage action = do
>   cursor <- openCursorTuned query iteratee seed resourceUsage
>   catchReaderT ( do
>       v <- action cursor
>       _ <- cursorClose cursor
>       return v
>     ) (\e -> do
>       _ <- cursorClose cursor
>       liftIO $ throwIO e
>     )


> withTransaction :: (MonadSession (ReaderT r IO) mb s, MonadIO (ReaderT r IO)) =>
>   IsolationLevel -> ReaderT r IO a -> ReaderT r IO a
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
>

--------------------------------------------------------------------
-- ** Misc.
--------------------------------------------------------------------

|Polymorphic over m.
Not needed by library user but moved into this module because it doesn't
depend on any DBMS implementation details.

> runfetch ::
>   ( Monad m
>   , MonadIO (t m)
>   , MonadQuery (t m) ms q bufferType
>   , QueryIteratee (t m) iterType seedType bufferType
>   , MonadTrans t
>   ) =>
>      (forall a. t m a -> m a) -- ^ into query
>   ->   t m a  -- ^ finaliser
>   -> [bufferType]  -- ^ list of buffers
>   -> (iterType -> seedType -> m seedType)  -- ^ self i.e. function taking iteratee and seed, returning same type as seed
>   -> iterType  -- ^ iteratee
>   -> seedType  -- ^ seed value
>   -> m seedType  -- ^ return value same type as seed

> runfetch down finalizers buffers self iteratee initialSeed = do
>   let
>     handle seed True = (down (iterApply buffers seed iteratee)) >>= handleIter
>     handle seed False = (down finalizers) >> return seed
>     handleIter (Right seed) = self iteratee seed
>     handleIter (Left seed) = (down finalizers) >> return seed
>   (down fetch1) >>= handle initialSeed


|Used by instances of DBType to throw an exception
when a null (Nothing) is returned.
Will work for any type, as you pass the fetch action in the fetcher arg.

> throwIfDBNull :: (Monad m,
>	            MonadQuery m ms q bufferType, DBType a m bufferType) =>
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
 
The idea is that you import the module specific to your DBMS implementation
e.g. "Database.Oracle.Enumerator", which re-exports the useful interface functions
from this module. Then:
 
 > -- sample code, doesn't necessarily compile
 > module MyDbExample is
 >
 > import Database.Oracle.Enumerator
 >
 > query1Iteratee :: (Monad m) => Int -> String -> Double -> IterAct m [(Int, String, Double)]
 > query1Iteratee a b c accum = result' $ (a, b, c):accum
 >
 > -- simple query, returning reversed list of rows.
 > query1 :: SessionQuery
 > query1 = do
 >   doQuery "select a, b, c from x" query1Iteratee []
 >
 > -- non-query actions. Use runSession to execute.
 > otherActions :: Session -> IO ()
 > otherActions session = do
 >   runSession ( do
 >       executeDDL "create table blah"
 >       executeDML "insert into blah ..."
 >       commit
 >     ) session
 >
 > main :: IO ()
 > main = do
 >   session <- connect "user" "password" "server"
 >   -- use runSession to execute query (it has type SessionQuery)
 >   r <- runSession query1 session
 >   putStrLn $ show r
 >   otherActions session
 >   disconnect session
 
@Session@ is an opaque type returned by the connect function.
@connect@ is only exported by the DBMS-specific module,
not from this module ('Database.Enumerator').
@Session@ is an instance of 'MonadSession',
and to use the 'MonadSession' functions you invoke them via runSession.
 
@SessionQuery@ is a convenient type synonym.
It's useful if you want to give type signatures to your functions
that do database stuff.


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
at least for the arguments if not the return type (which is determined
by the arguments).
There is a type synonym @IterAct@, which gives some small convenience in writing
type signatures for iteratee functions.


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
