|
Module      :  Database.InternalEnumerator

This is the interface between the middle Enumerator layer and the
low-level, Database-specific layer. This file is not exported to the end user.

Only the programmer for a new back-end needs to consult this file.

> {-# OPTIONS -fglasgow-exts #-}
> module Database.InternalEnumerator
>   (
>     -- * Session object.
>     ISession(..)
>    , Statement(..) {-- DBBind(..), --} -- Statement(..)
>    , IsolationLevel(..)
>    , Position
>    , IQuery(..)
>    , DBType(..)
>    , throwIfDBNull
>     -- * Exceptions and handlers
>     , DBException(..)
>     , throwDB,
>     , ColNum, RowNum
>   ) where

> import Data.Typeable
> import Control.Exception (throw, catchDyn, 
>		    dynExceptions, throwDyn, throwIO, bracket, Exception)
> import qualified Control.Exception (catch)

> data IsolationLevel =
>     ReadUncommitted
>   | ReadCommitted
>   | RepeatableRead
>   | Serialisable
>   | Serializable  -- ^ for alternative spellers
>   deriving Show

Position within the result set. Not for the end user.

> type Position = Int

Needed for exceptions

> type RowNum = Int
> type ColNum = Int 

--------------------------------------------------------------------
-- ** Exceptions and handlers
--------------------------------------------------------------------

If we can't derive Typeable then the following code should do the trick:
 > data DBException = DBError ...
 > dbExceptionTc :: TyCon
 > dbExceptionTc = mkTyCon "Database.Enumerator.DBException"
 > instance Typeable DBException where typeOf _ = mkAppTy dbExceptionTc []

> type SqlStateClass = String
> type SqlStateSubClass = String
> type SqlState = (SqlStateClass, SqlStateSubClass)

> data DBException
>   -- | DBMS error message.
>   = DBError SqlState Int String
>   | DBFatal SqlState Int String
>   -- | the iteratee function used for queries accepts both nullable (Maybe) and
>   -- non-nullable types. If the query itself returns a null in a column where a
>   -- non-nullable type was specified, we can't handle it, so DBUnexpectedNull is thrown.
>   | DBUnexpectedNull RowNum ColNum
>   -- | Thrown by cursor functions if you try to fetch after the end.
>   | DBNoData
>   deriving (Typeable, Show)


> throwDB :: DBException -> a
> throwDB = throwDyn

> -- dbBind :: (DBBind a ms q) => a -> q -> Position -> ms ()
> -- dbBind v = (\q p -> bindPos q v p)


--------------------------------------------------------------------
-- ** Session interface
--------------------------------------------------------------------

|The ISession class describes a database session to a particular
DBMS.  Oracle has its own Session object, SQLite has its own
session object (which maintains the connection handle to the database
engine and other related stuff). Session objects for different databases
normally have different types -- yet they all belong to the class ISession
so we can do generic operations like `commit', `executeDDL', etc. 
in a database-independent manner.
 
Session objects per se are created by database connection\/login functions.
 
The class ISession is thus an interface between low-level (and
database-specific) code and the Enumerator, database-independent
code.
The ISession class is NOT visible to the end user -- neither the class,
nor any of its methods.

The ISession class describes the mapping from connection object to
the session object. The connection object is created by the end user
(and this is how the end user tells which particulat back end he wants).
The session object is not accessible by the end user in any way.
Even the type of the session object should be hidden!

> class ISession sess where
>   disconnect :: sess -> IO ()
>   beginTransaction :: sess -> IsolationLevel -> IO ()
>   commit   :: sess -> IO ()
>   rollback :: sess -> IO ()


We can have several types of statements: just plain strings,
strings bundled with tuning parameters, prepared statements.
BTW, statement with unbound variables should have a different type
from that of the statement without bound variables or the statement
with all bound variables.

  freeStatement should not be a part of the general interface.
  Only prepared statements can be freed. So, freeStatement is not polymorphic.
  
   -- I think that we don't need prepareStatement and freeStatement,
   -- but I'll leave them in for now, just in case.
   prepareStatement :: QueryText -> QueryResourceUsage -> ms stmt
   freeStatement :: stmt -> ms ()


-- instances of Statements are defined by a particular database.


> class ISession sess => Statement stmt sess q | stmt sess -> q where
>   -- insert/update/delete; returns number of rows affected
>   executeDML :: sess -> stmt -> IO Int
>   -- DDL (create table, etc)
>   executeDDL :: sess -> stmt -> IO ()
>   makeQuery :: sess -> stmt -> IO q
>   -- PL/SQL blocks? Not in generic interface. ODBC can execute procedures; maybe later...
>   --executeProc :: stmt -> ms ()


|The class IQuery describes the class of query objects. Each
database (that is, each Session object) has its own Query object. 
We may assume that a Query object includes (at least, conceptually)
a (pointer to) a Session object, so a Query object determines the
Session object.
A back-end provides an instance (or instances) of IQuery.
The end user never seens the IQuery class (let alone its methods).

?? Can a session have several types of query objects?
Let's assume that it can: but a statement plus the session uniquely
determine the query,

Note that we explicitly use IO monad because we will have to explicitly
do FFI.

> class ISession sess => IQuery q sess | q -> sess
>   where
>   fetchOneRow :: q -> IO Bool  -- fetch one row
>   currentRowNum :: q -> IO Int


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
So, ISession (m) uniquely determines the buffer type (b)??
Or, actually, a query uniquely determines the buffer.

 
The class DBType is not used by the end-user.
It is used to tie up low-level database access and the enumerator.
A database-specific library must provide a set of instances for DBType.

> class DBType a q b | q -> b where
>   allocBufferFor :: a -> q -> Position -> IO b
>   fetchCol   :: q -> b -> IO a
>   freeBuffer :: b -> IO ()

 class DBBind a ms stmt | ms -> stmt where
   bindPos :: stmt -> a -> Position -> ms ()

|Used by instances of DBType to throw an exception
when a null (Nothing) is returned.
Will work for any type, as you pass the fetch action in the fetcher arg.

> {-
> throwIfDBNull :: (Monad mq, MonadQuery mq ms stmt b q, DBType a mq b) =>
>   b  -- ^ Buffer.
>   -> (b -> mq (Maybe a))  -- ^ Action to get (fetch) value from buffer; this is applied to buffer.
>   -> mq a  -- ^ If the value in the buffer is not null, it is returned.
> -}
> throwIfDBNull pos fetcher = do
>   v <- fetcher 
>   case v of
>     Nothing -> do
>       (row,col) <- pos
>       throwDB (DBUnexpectedNull row col)
>     Just m -> return m
