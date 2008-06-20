#!/usr/bin/env runhaskell 

import Distribution.PackageDescription
  ( PackageDescription(..), Library(..), BuildInfo(..), HookedBuildInfo
  , emptyHookedBuildInfo, writeHookedBuildInfo, emptyBuildInfo
  )
import Distribution.Package (Dependency(..))
--import Distribution.Version(Dependency(..))
import Distribution.Simple.Setup ( ConfigFlags(..) )
import Distribution.Simple
  ( defaultMainWithHooks, autoconfUserHooks, UserHooks(..), Args )
import Distribution.Simple.Program (findProgramOnPath, simpleProgram, Program(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Utils (warn, info, rawSystemStdout)

import qualified System.Info (os)
import System.Directory (canonicalizePath, removeFile)
import System.FilePath (splitFileName, combine)
import System.IO.Error (try)
import Data.Maybe (fromJust)
import Data.Monoid

{-
One install script to rule them all, and in the darkness build them...

Some of the code in this script is adapted from the various
HSQL Setup scripts, so some credit for it should go to Krasimir Angelov.
Not sure exactly what that means for our license;
does he have to appear in our license.txt?
(His code is also BSD3 licensed.)

See this page for useful notes on tagging and releasing:
  http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program

To-dos for Takusen:
 - use hsc2hs to create #define constants from header files,
   rather than hard-code them.
 - Blob support (and clob?).
 - FreeTDS back-end.
 - POP3 & IMAP back-ends?

 - Unwritten tests:
   * incorrect fold function (doesn't match result-set)


GHC compiler/linker options:

ODBC    : <none on Windows, because the .dll is in c:\windows\system32, and the headers are in ghc's include/mingw>
Postgres: -I"C:\Program Files\PostgreSQL\8.1\include" -lpq -L"C:\Program Files\PostgreSQL\8.1\bin"
Sqlite  : -I"C:\Program Files\sqlite" -lsqlite3 -L"C:\Program Files\sqlite"
Oracle  : -I"C:\Program Files\Oracle\OraHome817\oci\include" -loci -L"C:\Program Files\Oracle\OraHome817\bin"
Oracle  : -I"%ORACLE_HOME%\oci\include" -loci -L"%ORACLE_HOME%\bin"
-}

main = defaultMainWithHooks autoconfUserHooks
  { preConf=preConf, postConf=postConf
  , hookedPrograms = [pgConfigProgram, odbcConfigProgram, sqlite3Program, sqlplusProgram]
  }
  where
    preConf :: Args -> ConfigFlags -> IO HookedBuildInfo
    preConf args flags = do
      try (removeFile "Takusen.buildinfo")
      return emptyHookedBuildInfo
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConf args flags pkgdesc localbuildinfo = do
      let verbose = (configVerbose flags)
      let lbi = libBuildInfo (fromJust (library pkgdesc))
      let buildtools = buildTools lbi
      sqliteBI <- configSqlite3 verbose buildtools 
      pgBI <- configPG verbose buildtools
      oraBI <- configOracle verbose buildtools
      odbcBI <- configOdbc verbose buildtools
      let bi = mconcat [sqliteBI, pgBI, oraBI, odbcBI, Just lbi]
      writeHookedBuildInfo "Takusen.buildinfo" (bi, [])


makeConfig path libDir includeDir = do
  libDirs <- canonicalizePath (combine path libDir)
  includeDirs <- canonicalizePath (combine path includeDir)
  return
    (Just emptyBuildInfo
      { extraLibDirs = [libDirs], includeDirs = [includeDirs] })

isElem program buildtools =
  or (map match buildtools)
  where match (Dependency tool _) = (programName program) == tool

createConfigByFindingExe verbose buildtools desc program relativeFolder libDir includeDir = do
  if not (program `isElem` buildtools)
    then return Nothing
    else do
      mb_location <- programFindLocation program verbose
      case mb_location of
        Nothing -> warn verbose ("No " ++ desc ++ " library found") >> return Nothing
        Just location -> do
          path <- relativeFolder location
          info verbose ("Using " ++ desc ++ ": " ++ path)
          makeConfig path libDir includeDir


-- ODBCConf.exe - MDAC install actions - command line?
odbcConfigProgram = simpleProgram "odbcconf" 
-- ODBC Admin console - GUI program
--odbcConfigProgram = simpleProgram "odbcad32"
sqlplusProgram    = simpleProgram "sqlplus"
pgConfigProgram   = simpleProgram "pg_config"
sqlite3Program    = simpleProgram "sqlite3"



sameFolder path = return (fst (splitFileName path))
parentFolder path = canonicalizePath (fst (splitFileName path) ++ "/..")

configSqlite3 verbose buildtools =
   createConfigByFindingExe verbose buildtools "Sqlite" sqlite3Program sameFolder "" ""

configOracle verbose buildtools = 
  createConfigByFindingExe verbose buildtools "Oracle" sqlplusProgram parentFolder "bin" "oci/include"

-- On Windows the ODBC stuff is in c:\windows\system32, which is always in the PATH.
-- So I think we only need to pass -lodbc32.
-- The include files are already in the ghc/include/mingw folder.
-- FIXME: I don't know how this should look for unixODBC.

isWindows = System.Info.os == "mingw32" || System.Info.os == "windows"

configOdbc verbose buildtools | isWindows = do
  info verbose "Using ODBC: <on Windows => lib already in PATH>"
  return Nothing
configOdbc verbose buildtools = do
  --info verbose "Using odbc: <on *nix => assume lib already in PATH>"
  return Nothing

configPG verbose buildtools = do
  if not (pgConfigProgram `isElem` buildtools)
    then return Nothing
    else do
      mb_location <- programFindLocation pgConfigProgram verbose
      case mb_location of
        Nothing -> warn verbose ("No PostgreSQL library found") >> return Nothing
        Just pq_config_path -> do
          info verbose ("Using pq: " ++ pq_config_path)
          res <- rawSystemStdout verbose pq_config_path ["--libdir"]
          let lib_dirs = words res
          res <- rawSystemStdout verbose pq_config_path ["--includedir"]
          let inc_dirs = words res
          res <- rawSystemStdout verbose pq_config_path ["--includedir-server"]
          let inc_dirs_server = words res
          return ( Just emptyBuildInfo
            { extraLibDirs = lib_dirs
            , includeDirs = inc_dirs ++ inc_dirs_server
            })
