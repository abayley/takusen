-- Can't use "#!" with -cpp; get "invalid preprocessing directive #!"
-- #!/usr/bin/env runhaskell

{-# OPTIONS -cpp #-}

import Distribution.Simple
import Distribution.Simple.Build
import Distribution.Simple.Register
import Distribution.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.Configure (findProgram)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Setup
import Distribution.Compat.FilePath (splitFileName, joinPaths)
import System.Directory (removeFile, canonicalizePath)
import System.Process(runInteractiveProcess, waitForProcess)
import System.IO(hClose, hGetContents, hPutStr, stderr)
import System.Exit
import Control.Exception (try)
import Control.Monad
import Data.List

{-
One install script to rule them all, and in the darkness build them...

This script ought to work for both ghc-6.4 and ghc-6.6.
For ghc-6.6 you will need to first install Cabal-1.1.6.1;
the Cabal-1.1.6 build that comes with ghc-6.6 doesn't export certain stuff.

FIXME  The code in this script is adapted from the various HSQL Setup scripts,
so the credit for it should go to Krasimir Angelov.

Not sure exactly what that means for our license;
does he have to appear in our license.txt?
(His code is also BSD3 licensed.)


To-dos for Takusen:
 - find out why we have to import Database.Enumerator when it's
   in the export list for every database-specific module anyway.
 - use hsc2hs to create #define constants from header files,
   rather than hard-code them.
 - Blob support (and clob?).
 - ODBC back-end.
 - Sql Server back-end.

 - Unwritten tests:
   * incorrect fold function (doesn't match result-set)


GHC compiler/linker options:

Postgres: -I"C:\Program Files\PostgreSQL\8.1\include" -lpq -L"C:\Program Files\PostgreSQL\8.1\bin"
Sqlite  : -I"C:\Program Files\sqlite" -lsqlite3 -L"C:\Program Files\sqlite"
Oracle  : -I"C:\Program Files\Oracle\OraHome817\oci\include" -loci -L"C:\Program Files\Oracle\OraHome817\bin"
Oracle  : -I"%ORACLE_HOME%\oci\include" -loci -L"%ORACLE_HOME%\bin"
-}

main = defaultMainWithHooks defaultUserHooks
  { preConf=preConf, postConf=postConf, buildHook=buildHook }
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
    -- We patch in the buildHook so that we can modify the list of exposed
    -- modules (we remove modules for back-ends that are not installed).
    buildHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> BuildFlags -> IO ()
    buildHook pd lbi mbuh bf = defaultBuildHook (modifyPackageDesc pd) lbi mbuh bf

modifyPackageDesc pd =
  let
    Just (Library modules buildInf) = library pd
    filteredMods = filterModulesByLibs modules (extraLibs buildInf)
  in pd { library = Just (Library filteredMods buildInf) }

filterModulesByLibs modules libs =
  removeModulesForAbsentLib "pq" "Database.PostgreSQL" libs
  . removeModulesForAbsentLib "oci" "Database.Oracle" libs
  . removeModulesForAbsentLib "sqlite3" "Database.Sqlite" libs
  $ modules

removeModulesForAbsentLib lib prefix libs modules =
  if not (elem lib libs)
  then filter (not . isPrefixOf prefix) modules
  else modules



---------------------------------------------------------------------
-- Start of code copied verbatim from Distribution.Simple.
defaultBuildHook :: PackageDescription -> LocalBuildInfo
	-> Maybe UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags = do
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False

allSuffixHandlers :: Maybe UserHooks -> [PPSuffixHandler]
allSuffixHandlers hooks
    = maybe knownSuffixHandlers
      (\h -> overridesPP (hookedPreProcessors h) knownSuffixHandlers)
      hooks
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)
-- End of code copied verbatim from Distribution.Simple.
---------------------------------------------------------------------


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


#if mingw32_HOST_OS || mingw32_TARGET_OS
we're_on_Windows = True
#else
we're_on_Windows = False
#endif

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
