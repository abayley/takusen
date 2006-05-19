
|
Module      :  Database.Stub.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Exports just what you need from "Database.Stub.StubEnumerator".
 

> module Database.Stub.Enumerator
>   ( Session, ConnParm(..), connect, sql
>    , QueryResourceUsage(..), sql_tuned
>   )
> where
> import Database.Stub.StubEnumerator
