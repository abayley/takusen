#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Configure (LocalBuildInfo, findProgram)
import Distribution.Setup (ConfigFlags)
import Distribution.Compat.FilePath
import System.Directory (removeFile, canonicalizePath)
import Control.Exception (try)
import System.Exit (ExitCode(..))

{-
To-dos for Takusen:
fix PG installation to use pg_config
use hsc2hs to create #define constants from header files, rather than hard-code them.
fix network-byte-order marshalling in Database.PostgreSQL.PGFunctions
  (only works on little-endian architectures, like x86).
Tests for Oracle ResultSet cursors.
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
      sqliteBI <- configSqlite3
      pgBI <- configPG
      oraBI <- configOracle
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
    Nothing -> message ("No " ++ libName ++ " found") >> return Nothing
    Just location -> do
      path <- relativeFolder location
      message ("Using " ++ libName ++ ": " ++ path)
      makeConfig path libName libDir includeDir


configSqlite3 = createConfigByFindingExe "sqlite3" sameFolder "sqlite3" "" ""

configPG = createConfigByFindingExe "pg_config" parentFolder "pq" "bin" "include"

configOracle = createConfigByFindingExe "sqlplus" parentFolder "oci" "bin" "oci/include"

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
