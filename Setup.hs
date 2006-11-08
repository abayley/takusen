#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Configure (LocalBuildInfo, findProgram)
import Distribution.Setup
import Distribution.Compat.FilePath
import System.Directory (removeFile, canonicalizePath)
import System.Process(runInteractiveProcess, waitForProcess)
import System.IO(hClose, hGetContents, hPutStr, stderr)
import System.Exit
import Control.Exception (try)
import Control.Monad
import System.Exit (ExitCode(..))

{-
The code in this script is adapted from the various HSQL Setup scripts,
so the credit for it should go to Krasimir Angelov.

Not sure exactly what that means for our license;
does he have to appear in our license.txt?
(His code is also BSD3 licensed.)
-}


{-
To-dos for Takusen:
fix PG installation to use pg_config
GHC-6.6 update
use hsc2hs to create #define constants from header files, rather than hard-code them.
fix network-byte-order marshalling in Database.PostgreSQL.PGFunctions
  (only works on little-endian architectures, like x86).
Blob support (and clob?).
ODBC back-end.
Sql Server back-end.
-}

{-
PGSql : -I"C:\Program Files\PostgreSQL\8.1\include" -lpq -L"C:\Program Files\PostgreSQL\8.1\bin"
Sqlite: -I"C:\Program Files\sqlite" -lsqlite3 -L"C:\Program Files\sqlite"
Oracle: -I"C:\Program Files\Oracle\OraHome817\oci\include" -loci -L"C:\Program Files\Oracle\OraHome817\bin"
-}

main = defaultMainWithHooks defaultUserHooks{preConf=preConf, postConf=postConf}
  where
    preConf ::  [String] -> ConfigFlags -> IO HookedBuildInfo
    preConf args flags = do
      try (removeFile "takusen.buildinfo")
      return emptyHookedBuildInfo
    postConf :: [String] -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
    postConf args flags _ localbuildinfo = do
      let verbose = configVerbose flags
      sqliteBI <- configSqlite3 verbose
      pgBI <- configPG verbose
      oraBI <- configOracle verbose
      let bis = [sqliteBI, pgBI, oraBI]
      writeHookedBuildInfo "takusen.buildinfo" (concatBuildInfo bis,[])
      return ExitSuccess

sameFolder path = return (fst (splitFileName path))
parentFolder path = canonicalizePath (fst (splitFileName path) ++ "/..")

makeConfig path libName libDir includeDir = do
  libDirs <- canonicalizePath (joinPaths path libDir)
  includeDirs <- canonicalizePath (joinPaths path includeDir)
  return (Just emptyBuildInfo
        { extraLibs = [libName]
        , extraLibDirs = [libDirs]
        , includeDirs = [includeDirs]
        }
        )

message s = putStrLn ("configure: takusen: " ++ s)

createConfigByFindingExe exe relativeFolder libName libDir includeDir = do
  mb_location <- findProgram exe Nothing
  case mb_location of
    Nothing -> message ("No " ++ libName ++ " library found") >> return Nothing
    Just location -> do
      path <- relativeFolder location
      message ("Using " ++ libName ++ ": " ++ path)
      makeConfig path libName libDir includeDir


configSqlite3 verbose = createConfigByFindingExe "sqlite3" sameFolder "sqlite3" "" ""

configOracle verbose = createConfigByFindingExe "sqlplus" parentFolder "oci" "bin" "oci/include"

--configPG verbose = createConfigByFindingExe "pg_config" parentFolder "pq" "bin" "include"

configPG :: Int -> IO (Maybe BuildInfo)
configPG verbose = do
  mb_pq_config_path <- findProgram "pg_config" Nothing
  case mb_pq_config_path of
    Nothing -> message ("No pq library found") >> return Nothing
    Just pq_config_path -> do
      message ("Using pq: " ++ pq_config_path)
      res <- rawSystemGrabOutput verbose pq_config_path ["--libdir"]
      let lib_dirs = words res
      res <- rawSystemGrabOutput verbose pq_config_path ["--includedir"]
      let inc_dirs = words res
      res <- rawSystemGrabOutput verbose pq_config_path ["--includedir-server"]
      let inc_dirs_server = words res
      return ( Just emptyBuildInfo
        { extraLibs = ["pq"]
        , extraLibDirs = lib_dirs
        , includeDirs = inc_dirs ++ inc_dirs_server
        })


concatBuildInfo [] = Nothing
concatBuildInfo [x] = x
concatBuildInfo (bi:bis) = combineBuildInfo bi (concatBuildInfo bis)

combineBuildInfo Nothing Nothing = Nothing
combineBuildInfo mbi Nothing = mbi
combineBuildInfo Nothing mbi = mbi
combineBuildInfo (Just bi1) (Just bi2) =
  Just bi1
  { extraLibs = extraLibs bi1 ++ extraLibs bi2
  , extraLibDirs = extraLibDirs bi1 ++ extraLibDirs bi2
  , includeDirs = includeDirs bi1 ++ includeDirs bi2
  }

rawSystemGrabOutput :: Int -> FilePath -> [String] -> IO String
rawSystemGrabOutput verbose path args = do
  when (verbose > 0) (putStrLn (path ++ concatMap (' ':) args))
  (inp,out,err,pid) <- runInteractiveProcess path args Nothing Nothing
  exitCode <- waitForProcess pid
  if exitCode /= ExitSuccess
    then do
      errMsg <- hGetContents err
      hPutStr stderr errMsg
      exitWith exitCode
    else return ()
  hClose inp
  hClose err
  hGetContents out
  