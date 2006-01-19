
|
Module      :  Main
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple driver module, mainly for testing.
Import test modules and run test suites.
 
This project is hosted by Haskell-libs:
  <http://sf.net/projects/haskell-libs>
 
Source code for this project is at:
  <http://cvs.sf.net/viewcvs.py/haskell-libs/libs/takusen/>
 
Invoke main like this (assuming compiled to takusen.exe):
 
 > takusen stub noperf
 > takusen sqlite noperf "" "" dbname
 > takusen oracle noperf "" "" dbname  -- no username, so os-authenticated
 > takusen mssql noperf user paswd dbname


> module Main (main) where


> import Database.Sqlite.Test.Enumerator as Sqlite
> import Database.Oracle.Test.Enumerator as Oracle
> import Database.Test.MultiConnect as Multi
> import Database.Stub.Test.Enumerator as Stub
> --import Database.MSSqlServer.Test.Enumerator as MSSql
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
>   [ ("stub", Stub.runTest)
>   , ("sqlite", Sqlite.runTest)
>   --, ("mssql", MSSql.runTest)
>   , ("oracle", Oracle.runTest)
>   , ("multi", Multi.runTest)
>   ]
