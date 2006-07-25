
|
Module      :  Main
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple driver module, mainly for testing.
Imports test modules and runs test suites.
 
This project is now hosted at haskell.org:
 
@darcs get <http://darcs.haskell.org/takusen>@
 
Invoke main like this (assuming the compiled executable is called @takusen@):
 
 > takusen stub noperf
 > takusen sqlite noperf "" "" dbname
 > takusen oracle noperf "" "" dbname  -- no username, so os-authenticated
 > takusen mssql noperf user paswd dbname

GHC compiler/linker options:

Postgres: -I"C:\Program Files\PostgreSQL\8.1\include" -lpq -L"C:\Program Files\PostgreSQL\8.1\bin"
Sqlite  : -I"C:\Program Files\sqlite" -lsqlite3 -L"C:\Program Files\sqlite"
Oracle  : -I"C:\Program Files\Oracle\OraHome817\oci\include" -loci -L"C:\Program Files\Oracle\OraHome817\bin"
Oracle  : -I"%ORACLE_HOME%\oci\include" -loci -L"%ORACLE_HOME%\bin"

Unwritten tests:
 - various failure cases?
   * incorrect fold function (doesn't match result-set)


> module Main (main) where


> import Database.Sqlite.Test.Enumerator as Sqlite
> import Database.Oracle.Test.Enumerator as Oracle
> --import Database.Test.MultiConnect as Multi
> import Database.Stub.Test.Enumerator as Stub
> --import Database.MSSqlServer.Test.Enumerator as MSSql
> import Database.PostgreSQL.Test.Enumerator as PGSql
> import System.Environment (getArgs)
> import Database.Test.Performance as Perf


> main :: IO ()
> main = do
>   (impl:perf:args) <- getArgs
>   let runPerf = if perf == "perf" then Perf.RunTests else Perf.Don'tRunTests
>   case lookup impl backendTests of
>     Nothing -> putStrLn $ "No backend for " ++ impl ++ "."
>     Just test -> test runPerf args

> backendTests :: [(String, Perf.ShouldRunTests -> [String] -> IO ())]
> backendTests =
>   [ ("sqlite", Sqlite.runTest)
>   , ("pgsql", PGSql.runTest)
>   --, ("mssql", MSSql.runTest)
>   , ("oracle", Oracle.runTest)
>   --, ("multi", Multi.runTest)
>   , ("stub", Stub.runTest)
>   ]
