
|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Abstract database interface, providing a left-fold enumerator
and cursor operations.
 
?? Should we even mention the stuff below? The functions below are
part of the implementation rather than the interface. The enumerator
interface is what exported by this module.??
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
 
Note that there are a few functions that are exported from each backend
implementation which *are* exposed to the API user, and which are useful,
but are not (necessarily) in this module. They include:
 
 * connect (obviously DBMS specific)
 
 * prepareStmt
 
 * withPreparedStatement


> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

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
>     -- ** Bind Parameters
>
>     -- $usage_bindparms
>
>       DBM  -- The data constructor is not exported
>
>     , IE.IsolationLevel(..)
>     , IterResult, IterAct
>
>     -- * Exceptions and handlers
>     , DBException(..)
>     , catchDB, basicDBExceptionReporter, reportRethrow, catchDBError, ignoreDBError
>     , IE.throwDB
>
>     -- * Session monad.
>     , withSession, commit, rollback, beginTransaction
>     , executeCommand, execDDL, execDML
>
>     , PreparedStmt  -- data constructor not exported
>     , executePreparation, withPreparedStatement
>     , withBoundStatement, IE.bindP
>
>     , doQuery, IE.currentRowNum
>
>     -- * A Query monad and cursors.
>     , DBCursor, -- need to export this?
>     , cursorIsEOF, cursorCurrent, cursorNext, 
>     , withCursor
>
>     , withTransaction
>
>     -- * Misc.
>     , ifNull, result, result'
>     , liftIO, throwIO, print_
>   ) where

> import Data.Dynamic
> import Data.IORef
> import Control.Monad.Trans
> import Control.Exception (throw, 
>            dynExceptions, throwDyn, throwIO, bracket, Exception, finally)
> import qualified Control.Exception (catch)
> import Control.Monad.Reader
> import Control.Exception.MonadIO

> import qualified Database.InternalEnumerator as IE
> import Database.InternalEnumerator (DBException(..))


> type IterResult seedType = Either seedType seedType
> type IterAct m seedType = seedType -> m (IterResult seedType)

> class Show a => MyShow a where show_ :: a -> String
> instance MyShow String where show_ s = s
> instance (Show a) => MyShow a where show_ s = show s
> print_ s = liftIO (putStrLn (show_ s))


> catchDB :: CaughtMonadIO m => m a -> (DBException -> m a) -> m a
> catchDB action handler = gcatch action $ \e ->
>   maybe (throw e) handler (dynExceptions e >>= fromDynamic)


|This simple handler reports the error to @stdout@ and swallows it
i.e. it doesn't propagate.

> basicDBExceptionReporter :: CaughtMonadIO m => DBException -> m ()
> basicDBExceptionReporter (DBError (ssc, sssc) e m) =
>   liftIO $ putStrLn $ ssc ++ sssc ++ " " ++ (show e) ++ ": " ++ m
> basicDBExceptionReporter (DBFatal (ssc, sssc) e m) =
>   liftIO $ putStrLn $ ssc ++ sssc ++ " " ++ (show e) ++ ": " ++ m
> basicDBExceptionReporter (DBUnexpectedNull r c) =
>   liftIO $ putStrLn $ "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
> basicDBExceptionReporter (DBNoData) = liftIO $ putStrLn "Fetch: no more data."

| This handler reports the error and propagates it
(usually to force the program to halt).

> reportRethrow :: CaughtMonadIO m => DBException -> m ()
> reportRethrow e = basicDBExceptionReporter e >> IE.throwDB e

|If you want to trap a specific error number, use this.
It passes anything else up.

> catchDBError :: Int -> IO a -> (IE.DBException -> IO a) -> IO a
> catchDBError n action handler = catchDB action
>   (\dberror ->
>     case dberror of
>       DBError ss e m | e == n -> handler dberror
>       _ | otherwise -> IE.throwDB dberror
>   )

> ignoreDBError :: Int -> IO a -> IO a
> ignoreDBError n action = catchDBError n action (\e -> return undefined)

--------------------------------------------------------------------
-- ** Session monad
--------------------------------------------------------------------

The DBM data constructor is NOT exported. 

?? Investigate quantification over sess in |withSession|. We won't need
any mark then, I gather.
The quantification over Session is quite bothersome: need to enumerate
all class constraints for the Session (like IQuery, DBType, etc).

> newtype IE.ISession sess => DBM mark sess a = DBM (ReaderT sess IO a) 
>     deriving (Monad, MonadIO, MonadReader sess)
> unDBM (DBM x) = x

 instance Monad (DBM mark si) where
   return x = DBM (return x)
   m >>= f  = DBM (unDBM m >>= unDBM . f)
 instance MonadIO (DBM mark si) where
   liftIO x = DBM (liftIO x)

> instance CaughtMonadIO (DBM mark si) where
>   gcatch a h = DBM ( gcatch (unDBM a) (unDBM . h) )
>   gcatchJust p a h = DBM ( gcatchJust p (unDBM a) (unDBM . h) )

Typeable constraint is to prevent the leakage of Session and other
marked objects.

> withSession :: (Typeable a, IE.ISession sess) => 
>     IE.ConnectA sess -> (forall mark. DBM mark sess a) -> IO a
> withSession (IE.ConnectA connecta) m = 
>   bracket (connecta) (IE.disconnect) (runReaderT $ unDBM m)



> beginTransaction il = DBM (ask >>= \s -> lift $ IE.beginTransaction s il)
> commit :: IE.ISession s => DBM mark s ()
> commit = DBM( ask >>= lift . IE.commit )
> rollback :: IE.ISession s => DBM mark s ()
> rollback = DBM( ask >>= lift . IE.rollback )

> executeCommand :: IE.Command stmt s => stmt -> DBM mark s Int
> executeCommand stmt = DBM( ask >>= \s -> lift $ IE.executeCommand s stmt )

> execDDL :: IE.Command stmt s => stmt -> DBM mark s ()
> execDDL stmt = executeCommand stmt >> return ()

> execDML :: IE.Command stmt s => stmt -> DBM mark s Int
> execDML = executeCommand


--------------------------------------------------------------------
-- ** Statements; Prepared statements
--------------------------------------------------------------------

> newtype PreparedStmt mark stmt = PreparedStmt stmt
> executePreparation :: IE.IPrepared stmt sess bstmt bo =>
>        IE.PreparationA sess stmt -> DBM mark sess (PreparedStmt mark stmt)
> executePreparation (IE.PreparationA action) =
>     DBM( ask >>= \sess -> lift $ action sess >>= return . PreparedStmt)

Also include Typeable constraint to prevent type leakage,
assuming it's necessary here...?

> withPreparedStatement ::
>  (Typeable a, IE.IPrepared stmt sess bstmt bo)
>  => IE.PreparationA sess stmt
>  -> (PreparedStmt mark stmt -> DBM mark sess a)
>  -> DBM mark sess a
> withPreparedStatement pa action =
>   gbracket (executePreparation pa) destroyStmt action


> destroyStmt :: (IE.ISession sess, IE.IPrepared stmt sess bstmt bo)
>   => PreparedStmt mark stmt -> DBM mark sess ()
> destroyStmt (PreparedStmt stmt) = DBM( ask >>= \s -> lift $ IE.destroyStmt s stmt )

The Typeable constraint is to prevent the leakage of marked things.
The type of bound statements should not be exported (and should not be
in Typeable) so the bound statement can't leak either.

> withBoundStatement ::
>   (Typeable a, IE.IPrepared stmt s bstmt bo) =>
>   PreparedStmt mark stmt -> [IE.BindA s stmt bo] ->
>   (bstmt -> DBM mark s a) ->
>   DBM mark s a
> withBoundStatement (PreparedStmt stmt) ba f =
>   DBM ( ask >>= \s -> 
>     lift $ IE.bindRun s stmt ba (\b -> runReaderT (unDBM (f b)) s))


--------------------------------------------------------------------
-- ** Buffers and QueryIteratee
--------------------------------------------------------------------


|The class QueryIteratee is not for the end user. It provides the
interface between the low- and the middle-layers of Takusen. The
middle-layer -- enumerator -- is database-independent then.

> class MonadIO m => QueryIteratee m q i seed b |
>     i -> m, i -> seed, q -> b where
>   iterApply ::    q -> [b] -> seed -> i -> m (IterResult seed)
>   allocBuffers :: q -> i -> IE.Position -> m [b]

|This instance of the class is the terminating case
i.e. where the iteratee function has one argument left.
The argument is applied, and the result returned.

> instance (IE.DBType a q b, MonadIO m) =>
>   QueryIteratee m q (a -> seed -> m (IterResult seed)) seed b where
>   iterApply q [buf] seed fn  = do
>     v <- liftIO $ IE.fetchCol q buf
>     fn v seed
>   allocBuffers q _ n = liftIO $ 
>         sequence [IE.allocBufferFor (undefined::a) q n]


|This instance of the class implements the starting and continuation cases.

> instance (QueryIteratee m q i' seed b, IE.DBType a q b)
>     => QueryIteratee m q (a -> i') seed b where
>   iterApply q (buffer:moreBuffers) seed fn = do
>     v <- liftIO $ IE.fetchCol q buffer
>     iterApply q moreBuffers seed (fn v)
>   allocBuffers q fn n = do
>     buffer <- liftIO $ IE.allocBufferFor (undefined::a) q n
>     moreBuffers <- allocBuffers q (undefined::i') (n+1)
>     return (buffer:moreBuffers)



--------------------------------------------------------------------
-- ** A Query monad and cursors
--------------------------------------------------------------------


> type CollEnumerator i m s = i -> s -> m s
> type Self           i m s = i -> s -> m s
> type CFoldLeft      i m s = Self i m s -> CollEnumerator i m s

|A DBCursor is an IORef-mutable-pair @(a, Maybe f)@, where @a@ is the result-set so far,
and @f@ is a function that fetches and returns the next row (when applied to True),
or closes the cursor (when applied to False).
If @Maybe@ f is @Nothing@, then the result-set has been exhausted
(or the iteratee function terminated early),
and the cursor has already been closed.
 
> newtype DBCursor mark ms a =
>     DBCursor (IORef (a, Maybe (Bool-> ms (DBCursor mark ms a))))


The following functions provide the high-level query interface,
for the end user.

> doQuery :: (IE.Statement stmt sess q,
>             QueryIteratee (DBM mark sess) q i seed b,
>             IE.IQuery q sess b) =>
>     stmt -> i -> seed -> DBM mark sess seed
> doQuery stmt iteratee seed = do
>   (lFoldLeft, finalizer) <- doQueryMaker stmt iteratee
>   gcatch (fix lFoldLeft iteratee seed)
>       (\e -> do
>         finalizer
>         liftIO $ throw e
>       )


|This is the auxiliary function.

> doQueryMaker stmt iteratee = do
>     sess <- ask
>     query <- liftIO $ IE.makeQuery sess stmt
>     buffers <- allocBuffers query iteratee 1
>     let
>       finaliser = liftIO (mapM_ (IE.freeBuffer query) buffers) >>
>                   liftIO (IE.destroyQuery query)
>       hFoldLeft self iteratee initialSeed = do
>         let
>           handle seed True = iterApply query buffers seed iteratee
>             >>= handleIter
>           handle seed False = (finaliser) >> return seed
>           handleIter (Right seed) = self iteratee seed
>           handleIter (Left seed) = (finaliser) >> return seed
>         liftIO (IE.fetchOneRow query) >>= handle initialSeed
>     return (hFoldLeft, finaliser)


Cursor stuff. First, an auxiliary function, not seen by the user.

> openCursor stmt iteratee seed = do
>     ref <- liftIO$ newIORef (seed,Nothing)
>     (lFoldLeft, finalizer) <- doQueryMaker stmt iteratee
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
 

Note that the type of the functions below is set up so to perpetuate
the mark.

> cursorIsEOF :: DBCursor mark (DBM mark s) a -> DBM mark s Bool
> cursorIsEOF (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   return $ maybe True (const False) maybeF

|Returns the results fetched so far, processed by iteratee function.

> cursorCurrent :: DBCursor mark (DBM mark s) a -> DBM mark s a
> cursorCurrent (DBCursor ref) = do
>   (v, _) <- liftIO $ readIORef ref
>   return v

|Advance the cursor. Returns the cursor. The return value is usually ignored.

> cursorNext :: DBCursor mark (DBM mark s) a
>     -> DBM mark s (DBCursor mark (DBM mark s) a)
> cursorNext (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   maybe (IE.throwDB DBNoData) ($ True) maybeF


Returns the cursor. The return value is usually ignored.
This function is not available to the end user. The cursor is closed
automatically when its region exits. 

> cursorClose c@(DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   maybe (return c) ($ False) maybeF


|Ensures cursor resource is properly tidied up in exceptional cases.
Propagates exceptions after closing cursor.
The Typeable constraint is to prevent cursors and other marked values
(like cursor computations) from escaping.

> withCursor ::
>   ( Typeable a, IE.Statement stmt sess q
>   , QueryIteratee (DBM mark sess) q i seed b
>   , IE.IQuery q sess b
>   ) =>
>      stmt -> i -> seed
>   -> (DBCursor mark (DBM mark sess) seed -> DBM mark sess a)
>   -> DBM mark sess a
> withCursor stmt iteratee seed action =
>   gbracket (openCursor stmt iteratee seed) cursorClose action


Although withTransaction has the same structure as a bracket,
we can't use bracket because the resource-release action
(commit or rollback) differs between the success and failure cases.

|Perform an action as a transaction: commit afterwards,
unless there was an exception, in which case rollback.

> withTransaction :: (IE.ISession s) =>
>   IE.IsolationLevel -> DBM mark s a -> DBM mark s a
> 
> withTransaction isolation action = do
>     beginTransaction isolation
>     gcatch ( do
>         v <- action
>         commit
>         return v
>       ) (\e -> rollback >> throw e )


--------------------------------------------------------------------
-- ** Misc.
--------------------------------------------------------------------


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
 
 > -- sample code, doesn't necessarily compile
 > module MyDbExample is
 >
 > import Database.Oracle.Enumerator
 > import Database.Enumerator
 > ...
 >
 > query1Iteratee :: (Monad m) => Int -> String -> Double -> IterAct m [(Int, String, Double)]
 > query1Iteratee a b c accum = result' ((a, b, c):accum)
 >
 > -- non-query actions.
 > otherActions session = do
 >   executeDDL "create table blah"
 >   executeDML "insert into blah ..."
 >   commit
 >   -- Use withTransaction to delimit a transaction.
 >   -- It will commit at the end, or rollback if an error occurs.
 >   withTransaction Serialisable $ do
 >     execDML "update blah ..."
 >     execDML "insert into blah ..."
 >
 > main :: IO ()
 > main = do
 >   withSession (connect "user" "password" "server") $ do
 >     -- simple query, returning reversed list of rows.
 >     r <- doQuery (sql "select a, b, c from x") query1Iteratee []
 >     putStrLn $ show r
 >     otherActions session
 >     disconnect
 
@connect@ is only exported by the DBMS-specific module,
not from this module ('Database.Enumerator').
 
The first argument to doQuery must be an instance of  
'Database.InternalEnumerator.Statement'.
Each back-end will provide a useful set of @Statement@ instances
and associated constructor functions for them.
For example, the Sqlite back-end has:
  -- for basic, all-text statements (no bind vars):
    sql "select ..."
  -- for a select with bind variables and row caching:
    prefetch 100 "select ..." [bindP ..., bindP ...]
  -- for a reusable prepared statement:
    prepareStmt (sql "select ...")
  -- The Postgres backend requires that you name prepared statements,
  -- and also specify types for the bind parameters.
  -- The list of bind-types is created by applying the bindType functions
  -- to dummy, or sample, values of the correct types.
    prepareStmt "stmtname" (sql "select ...") [bindType dummy1, ...]
 
There are a couple of withXXX functions to aid with resource management.
withPreparedStatement creates a prepared statement that can be reused many
times within its scope, and deallocates it from the server when it goes
out of scope.
withBoundStatement applies bind values to an already prepared statement
so that it is ready for processing by doQuery.

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
 
The iteratee function exists in the 'DBM' monad,
so if you want to do IO in it you must use 'Control.Monad.Trans.liftIO'
(e.g. @liftIO $ putStrLn \"boo\"@ ) to lift the IO action into 'DBM'.
 
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


-- $usage_bindparms
 
Bind variables are specified by using the 'doQueryBind' or 'withCursorBind'
interface functions. The bind parameters are a list of bind actions
@s -> Position -> m ()@ .
The 'doQueryBind'\/'withCursorBind' functions invoke each
action, passing the statement object and position in the list.
Having the actions return () allows us to create lists of binds to
values of different types.
 
Perhaps an example will explain it better:
 
 > bindExample sess = do
 >   let
 >     query = "select blah from blahblah where id = ? and code = ?"
 >     iter :: (Monad m) => String -> IterAct m [String]
 >     iter s acc = result $ s:acc
 >     bindVals = [dbBind (12345::Int), dbBind "CODE123"]
 >   actual <- runSession sess (doQueryBind query bindVals iter [])
 >   print actual
 
More examples can be found in "Database.Test.Enumerator";
see 'Database.Test.Enumerator.selectBindInt',
'Database.Test.Enumerator.selectBindIntDoubleString', and
'Database.Test.Enumerator.selectBindDate'.
 
For Int and Double bind values, we have to tell the compiler about the types.
This is due to interaction (which I don't fully understand and therefore
cannot explain in any detail) with the numeric literal defaulting mechanism.
Note that for non-numeric literals the compiler can determine the correct
types to use.
 
If you omit type information for numeric literals, from GHC the error
message looks something like this:
 
 > Database\/Sqlite\/Test\/Enumerator.lhs:28:5:
 >    Overlapping instances for DBType a4
 >                                     Database.Sqlite.SqliteEnumerator.SqliteMonadQuery
 >                                     Database.Sqlite.SqliteEnumerator.ColumnBuffer
 >      arising from use of `Database.Test.Enumerator.runTests' at Database\/Sqlite\/Test\/Enumerator.lhs:28:5-17
 >    Matching instances:
 >      Imported from Database.Sqlite.SqliteEnumerator:
 >        instance (DBType (Maybe a)
 >                         Database.Sqlite.SqliteEnumerator.SqliteMonadQuery
 >                         Database.Sqlite.SqliteEnumerator.ColumnBuffer) =>
 >                 DBType a
 >                        Database.Sqlite.SqliteEnumerator.SqliteMonadQuery
 >                        Database.Sqlite.SqliteEnumerator.ColumnBuffer
 >      Imported from Database.Sqlite.SqliteEnumerator:
 >        instance DBType (Maybe Int)
 >                        ....





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

