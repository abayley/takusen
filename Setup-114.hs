-- #!/usr/bin/env runhaskell 

{-# OPTIONS -cpp #-}
{-# LANGUAGE CPP #-}

-- Can't use "#!" with -cpp; get "invalid preprocessing directive #!"

-- This Setup script is for cabal-1.1.4.

import Distribution.Setup (InstallUserFlag(..))
import Distribution.Simple
import Distribution.Simple.Build (build)
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Install (install)
import Distribution.Simple.Register	(register, writeInstalledConfig)
import Distribution.PreProcess (PPSuffixHandler, knownSuffixHandlers)
import Distribution.PackageDescription (HookedBuildInfo, BuildInfo
  , emptyHookedBuildInfo, writeHookedBuildInfo, emptyBuildInfo
  , extraLibs, extraLibDirs, includeDirs)
import Distribution.Simple.Configure (findProgram)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Setup (ConfigFlags, configVerbose)
import Distribution.PackageDescription
  ( HookedBuildInfo, emptyHookedBuildInfo, PackageDescription
  , writeHookedBuildInfo, Library(..), library, extraLibs
  , hasLibs, emptyBuildInfo, extraLibDirs, includeDirs, BuildInfo(..)
  )
import Distribution.Setup (ConfigFlags, configVerbose
  , InstallFlags(..), CopyFlags(..), CopyDest(..)
--  , emptyRegisterFlags, regUser, regVerbose
  )
import System.Directory (removeFile, getTemporaryDirectory, canonicalizePath)
import System.Process(runProcess, waitForProcess)
import System.IO(hClose, openFile, hGetLine, hIsEOF, openTempFile, IOMode(..))
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (splitFileName, combine)
import Control.Exception (try, bracket)
import Control.Monad (when)
import Data.List (isPrefixOf, unionBy)


main = defaultMainWithHooks defaultUserHooks
  { preConf=preConf, postConf=postConf, buildHook=buildHook, instHook=installHook }
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
      odbcBI <- configOdbc verbose
      let bis = [sqliteBI, pgBI, oraBI, odbcBI]
      writeHookedBuildInfo "takusen.buildinfo" (concatBuildInfo bis,[])
      return ExitSuccess
    -- We patch in the buildHook so that we can modify the list of exposed
    -- modules (we remove modules for back-ends that are not installed).
    buildHook :: PackageDescription -> LocalBuildInfo -> Int -> [PPSuffixHandler] -> IO ()
    buildHook pd lbi mbuh bf = defaultBuildHook (modifyPackageDesc pd) lbi mbuh bf
    -- also patch the installHook
    installHook :: PackageDescription -> LocalBuildInfo -> Int -> InstallUserFlag -> IO ()
    installHook pd lbi mbuh insf = defaultInstallHook (modifyPackageDesc pd) lbi mbuh insf

modifyPackageDesc pd =
  let
    Just (Library modules buildInf) = library pd
    filteredMods = filterModulesByLibs modules (extraLibs buildInf)
  in pd { library = Just (Library filteredMods buildInf) }

filterModulesByLibs modules libs =
  removeModulesForAbsentLib "pq" "Database.PostgreSQL" libs
  . removeModulesForAbsentLib "oci" "Database.Oracle" libs
  . removeModulesForAbsentLib "sqlite3" "Database.Sqlite" libs
  . filterODBCModules libs
  $ modules

removeModulesForAbsentLib lib prefix libs modules =
  if not (elem lib libs)
  then filter (not . isPrefixOf prefix) modules
  else modules

filterODBCModules libs modules =
  if not (elem "odbc" libs || elem "odbc32" libs)
  then filter (not . isPrefixOf "Database.ODBC") modules
  else modules

---------------------------------------------------------------------
-- Start of code copied verbatim from Distribution.Simple,
-- because it's not exported.

defaultInstallHook pkg_descr localbuildinfo verbose uInstFlag = do
  let uInst = case uInstFlag of
               InstallUserUser   -> True
               InstallUserGlobal -> False --over-rides configure setting
               -- no flag, check how it was configured:
               InstallUserNone   -> userConf localbuildinfo
  install pkg_descr localbuildinfo (NoCopyDest, verbose)
  when (hasLibs pkg_descr)
           (register pkg_descr localbuildinfo (uInst, False, verbose))

defaultBuildHook pkg_descr localbuildinfo flags pps = do
  build pkg_descr localbuildinfo flags pps
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo

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

{-
dropLastElementFromPath path = reverse . dropWhile (\c -> c /= '\\' && c /= '/') . reverse $ path
joinPaths path item =
  let lastChar = last path
  in
  if lastChar == '\\' || lastChar == '/'
  then path ++ item
  else path ++ "/" ++ item
sameFolder path = return (dropLastElementFromPath path)
parentFolder path = canonicalizePath (dropLastElementFromPath (dropLastElementFromPath path))
-}

joinPaths = combine
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

createConfigByFindingExe desc exe relativeFolder libName libDir includeDir = do
  mb_location <- findProgram exe Nothing
  case mb_location of
    Nothing -> message ("No " ++ desc ++ " (" ++ libName ++ ") library found") >> return Nothing
    Just location -> do
      path <- relativeFolder location
      message ("Using " ++ libName ++ ": " ++ path)
      makeConfig path libName libDir includeDir


configSqlite3 verbose = createConfigByFindingExe "Sqlite" "sqlite3" sameFolder "sqlite3" "" ""

configOracle verbose = createConfigByFindingExe "Oracle" "sqlplus" parentFolder "oci" "bin" "oci/include"

-- On Windows the ODBC stuff is in c:\windows\system32, which is always in the PATH.
-- So I think we only need to pass -lodbc32.
-- The include files are already in the ghc/include/mingw folder.
-- FIXME: I don't know how this should look for unixODBC.

#ifdef mingw32_HOST_OS
configOdbc verbose = do
  message ("Using odbc: <on Windows => lib already in PATH>")
  return ( Just emptyBuildInfo { extraLibs = ["odbc32"] })
#else
configOdbc verbose = createConfigByFindingExe "ODBC" "sqlplus" parentFolder "odbc" "" ""
#endif

configPG :: Int -> IO (Maybe BuildInfo)
configPG verbose = do
  mb_pq_config_path <- findProgram "pg_config" Nothing
  case mb_pq_config_path of
    Nothing -> message ("No PostgreSQL (pq) library found") >> return Nothing
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

concatBuildInfo = foldr1 combineBuildInfo 

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
  when (verbose > 0) . putStrLn . unwords $ path:args
  tmp_dir <- getTemporaryDirectory
  (outf,outh) <- openTempFile tmp_dir "out.dat"
  -- process' stderr goes to our stderr
  pid <- runProcess path args Nothing Nothing Nothing (Just outh) Nothing
  exitCode <- waitForProcess pid
  when (exitCode /= ExitSuccess) $ exitWith exitCode
  file_to_contents outf

-- properly, this time, with no lazy IO
file_to_contents fname = 
   bracket (openFile fname ReadMode) 
	   (\h -> hClose h >> removeFile fname) 
	   (reader [])
 where
 reader acc h = do
		eof <- hIsEOF h
		if eof then return . concat $ reverse acc
		   else do
			l <- hGetLine h
			reader (" ":l:acc) h
