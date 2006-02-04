|
Module      :  Database.InternalEnumerator

This is the interface between the middle Enumerator layer and the
low-level, Database-specific layer. This file is not exported to the end user.

Only the programmer for the new back-end needs to consult this file.

> {-# OPTIONS -fglasgow-exts #-}
> module Database.InternalEnumerator
>   (
>     -- * Session object.
>     ISession(..)
>    , Statement(..) {-- DBBind(..), --} -- Statement(..)
>    , IsolationLevel(..)
>   ) where


> data IsolationLevel =
>     ReadUncommitted
>   | ReadCommitted
>   | RepeatableRead
>   | Serialisable
>   | Serializable  -- ^ for alternative spellers
>   deriving Show


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
-- At least there should be an instance for a string 


> class ISession sess => Statement stmt sess where
>   -- insert/update/delete; returns number of rows affected
>   executeDML :: sess -> stmt -> IO Int
>   -- DDL (create table, etc)
>   executeDDL :: sess -> stmt -> IO ()
>   -- PL/SQL blocks? Not in generic interface. ODBC can execute procedures; maybe later...
>   --executeProc :: stmt -> ms ()
