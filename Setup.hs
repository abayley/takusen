#!/usr/bin/env runhaskell 

import Distribution.PackageDescription
  ( PackageDescription(..), Library(..), BuildInfo(..), HookedBuildInfo
  , emptyHookedBuildInfo, emptyBuildInfo
  )
import Distribution.PackageDescription.Parse ( writeHookedBuildInfo ) 
import Distribution.Package (Dependency(..))
import Distribution.Simple.Setup ( ConfigFlags(..), BuildFlags(.. ))
import Distribution.Simple
  ( defaultMainWithHooks, autoconfUserHooks, UserHooks(..), Args )
import Distribution.Simple.Program (findProgramOnPath, simpleProgram, Program(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Utils (warn, info, rawSystemStdout)
import Distribution.Verbosity (Verbosity, verbose)

import qualified System.Info (os)
import System.Directory (canonicalizePath, removeFile)
import System.Environment (getEnv)
import System.FilePath (combine, dropFileName, FilePath)
import System.IO.Error (try)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)

{-
One install script to rule them all, and in the darkness build them...

See this page for useful notes on tagging and releasing:
  http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program

To-dos for Takusen:
 - Oracle resource leak: Ref Cursors (StmtHandles) not freed
 - Out bind parameters for ODBC
 - Multiple result-sets from ODBC procedure call
 - better result-set <-> iteratee validation. Check column types?
 - use hsc2hs to create #define constants from header files,
   rather than hard-code them.
 - Blob support (and clob?).
 - FreeTDS back-end.
 - POP3 & IMAP back-ends?

 - Unwritten tests:
   * incorrect fold function (doesn't match result-set)

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
      let lbi = libBuildInfo (fromJust (library pkgdesc))
      let buildtools = buildTools lbi
      sqliteBI <- configSqlite3 verbose buildtools 
      pgBI <- configPG verbose buildtools
      oraBI <- configOracle verbose buildtools
      odbcBI <- configOdbc verbose buildtools
      let bi = mconcat [sqliteBI, pgBI, oraBI, odbcBI, Just lbi]
      writeHookedBuildInfo "Takusen.buildinfo" (bi, [])


-- ODBCConf.exe - MDAC install actions - command line?
odbcConfigProgram = simpleProgram "odbcconf" 
-- ODBC Admin console - GUI program
--odbcConfigProgram = simpleProgram "odbcad32"
sqlplusProgram    = simpleProgram "sqlplus"
pgConfigProgram   = simpleProgram "pg_config"
sqlite3Program    = simpleProgram "sqlite3"


isWindows = System.Info.os == "mingw32" || System.Info.os == "windows"

makeConfig path libDir includeDir = do
  libDirs <- canonicalizePath (combine path libDir)
  includeDirs <- canonicalizePath (combine path includeDir)
  return
    (Just emptyBuildInfo
      { extraLibDirs = [libDirs], includeDirs = [includeDirs] })

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv env = do
  catch ( getEnv env >>= return . Just ) ( const (return Nothing) )

-- Check that the program is in the buildtools.
-- If it is, then run the action (which should return Maybe BuildInfo).
-- If not, return Nothing.
guardProg :: Program -> [Dependency] -> IO (Maybe BuildInfo) -> IO (Maybe BuildInfo)
guardProg prog tools action = do
  if prog `isElem` tools then action else return Nothing
  where
    isElem program buildtools = or (map (match program) buildtools)
    match program (Dependency tool _) = (programName program) == (show tool)

-- Run the first action to give a Maybe FilePath.
-- If this is Nothing then emit a warning about library not found.
-- Otherwise, run the second action over the FilePath.
guardPath :: (IO (Maybe FilePath)) -> String -> Verbosity -> (FilePath -> IO (Maybe BuildInfo)) -> IO (Maybe BuildInfo)
guardPath pathAction libName verbose resAction = do
  mb <- pathAction
  case mb of
    Nothing -> warn verbose ("No " ++libName++ " library found") >> return Nothing
    Just path -> info verbose ("Using " ++libName++ ": " ++ path) >> resAction path

-- From the Oracle 10g manual:
--
-- Appendix D - Getting Started with OCI for Windows:
--   Compiling OCI Applications for Windows:
-- http://download.oracle.com/docs/cd/B19306_01/appdev.102/b14250/ociadwin.htm#i634569
-- Header files are in: ORACLE_BASE\ORACLE_HOME\oci\include
-- DLLs are in: ORACLE_BASE\ORACLE_HOME\bin
--
-- For Unix:
-- Appendix B - OCI Demonstration Programs:
-- http://download.oracle.com/docs/cd/B19306_01/appdev.102/b14250/ociabdem.htm#i459676
-- Header files are in: $ORACLE_HOME/rdbms/public
-- Header files are in: $ORACLE_HOME/lib

configOracle verbose buildtools = do
  guardProg sqlplusProgram buildtools $ do
  guardPath (maybeGetEnv "ORACLE_HOME") "Oracle" verbose $ \path -> do
  let (libDir, incDir) =
          if isWindows then ("bin", "oci/include") else ("lib", "rdbms/public")
  makeConfig path libDir incDir

configSqlite3 verbose buildtools = do
  guardProg sqlite3Program buildtools $ do
    if isWindows
      then guardPath (programFindLocation sqlite3Program verbose) "Sqlite3" verbose $ \path -> do
        makeConfig (dropFileName path) "" ""
      else return Nothing

configPG verbose buildtools = do
  guardProg sqlplusProgram buildtools $ do
  guardPath (programFindLocation pgConfigProgram verbose) "PostgreSQL" verbose $ \pq_config_path -> do
  lib_dirs <- rawSystemStdout verbose pq_config_path ["--libdir"]
  inc_dirs <- rawSystemStdout verbose pq_config_path ["--includedir"]
  inc_dirs_server <- rawSystemStdout verbose pq_config_path ["--includedir-server"]
  return ( Just emptyBuildInfo
    { extraLibDirs = words lib_dirs
    , includeDirs = words inc_dirs ++ words inc_dirs_server
    })


-- On Windows the ODBC stuff is in c:\windows\system32, which is always in the PATH.
-- So I think we only need to pass -lodbc32.
-- The include files are already in the ghc/include/mingw folder.
-- FIXME: I don't know how this should look for unixODBC.

configOdbc verbose buildtools | isWindows = do
  info verbose "Using ODBC: <on Windows => lib already in PATH>"
  return Nothing
configOdbc verbose buildtools = do
  --info verbose "Using odbc: <on *nix => assume lib already in PATH>"
  return Nothing
