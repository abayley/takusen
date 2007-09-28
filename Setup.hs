-- #!/usr/bin/env runhaskell 

{-# OPTIONS -cpp #-}
{-# LANGUAGE CPP #-}

-- Can't use "#!" with -cpp; get "invalid preprocessing directive #!"

import SetupAux
import Bird2Hs (bird2hs)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Distribution.Compiler (Compiler(..))
import Distribution.Package ( showPackageId, PackageIdentifier(..) )
import Distribution.PackageDescription
  ( PackageDescription(..)
  , BuildInfo(..), emptyBuildInfo
  , HookedBuildInfo, emptyHookedBuildInfo, writeHookedBuildInfo
  , Library(..), withLib, setupMessage
  , haddockName, hasLibs
  )
import Distribution.PreProcess
  ( preprocessSources, ppCpp'
  )
import Distribution.Program
  ( programName, haddockProgram, lookupProgram, rawSystemProgram
  , programArgs )
import Distribution.Setup (ConfigFlags, configVerbose, BuildFlags
  , InstallFlags(..), HaddockFlags(..)
  )
import Distribution.Simple
  ( defaultMainWithHooks, defaultUserHooks, UserHooks(..), Args
  , hookedPreProcessors )
import Distribution.Simple.Configure (findProgram)
import Distribution.Simple.LocalBuildInfo
  ( withPrograms, LocalBuildInfo(..), buildDir, packageDeps )
import Distribution.Simple.Utils (moduleToFilePath, die, haddockPref)
import Language.Haskell.Extension (Extension(..))
import System.Directory
  ( removeFile, getTemporaryDirectory, canonicalizePath
  , doesFileExist, removeFile
  , getPermissions, setPermissions
  , doesDirectoryExist, createDirectory
  , getDirectoryContents, removeDirectory
  )
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
  ( splitFileName, dropFileName, combine, joinPath
  , splitExtension, addExtension, replaceExtension, takeDirectory )
import System.IO(hClose, openFile, hGetLine, hIsEOF
  , openTempFile, IOMode(..) )
import System.IO.Error (try, ioError)
import System.Process(runProcess, waitForProcess)

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
 - ODBC back-end.
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

main = defaultMainWithHooks defaultUserHooks
  { preConf=preConf, postConf=postConf
  , buildHook=buildHook, instHook=installHook
  , haddockHook = haddockHook2
  , postHaddock = postHaddock
  }
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
    buildHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> BuildFlags -> IO ()
    buildHook pd lbi mbuh bf = defaultBuildHook (modifyPackageDesc pd) lbi mbuh bf
    -- also patch the installHook
    installHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> InstallFlags -> IO ()
    installHook pd lbi mbuh insf = defaultInstallHook (modifyPackageDesc pd) lbi mbuh insf
    --
    haddockHook2 :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> HaddockFlags -> IO ()
    haddockHook2 pkg_descr lbi hooks flags = do
      withLib pkg_descr () $ \lib -> do
        let fp :: [FilePath]; fp = hsSourceDirs (libBuildInfo lib)
        sequence_ (map (createLhsForHs fp) ((exposedModules lib) ++ (otherModules (libBuildInfo lib))))
      haddock pkg_descr lbi hooks flags
    --
    postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
    postHaddock args flags pkg_descr lbi = do
      withLib pkg_descr () $ \lib -> do
        let fp :: [FilePath]; fp = hsSourceDirs (libBuildInfo lib)
        sequence_ (map (removeHsForLhs fp) ((exposedModules lib) ++ (otherModules (libBuildInfo lib))))
      return ExitSuccess


createLhsForHs :: [FilePath] -> String -> IO ()
createLhsForHs searchLoc modname = do
  srcFiles <- moduleToFilePath searchLoc modname ["lhs"]
  case srcFiles of
    [] -> return ()
    (lhsFile:_) -> do
      let (srcStem, ext) = splitExtension lhsFile
      let hsFile = addExtension srcStem "hs"
      hsExists <- doesFileExist hsFile
      when (not hsExists) (putStrLn ("Setup: unlit " ++ lhsFile))
      when (not hsExists) (bird2hs lhsFile hsFile)
      return ()

removeHsForLhs :: [FilePath] -> String -> IO ()
removeHsForLhs searchLoc modname = do
  srcFiles <- moduleToFilePath searchLoc modname ["hs"]
  case srcFiles of
    [] -> return ()
    (hsFile:_) -> do
      let (srcStem, ext) = splitExtension hsFile
      let lhsFile = addExtension srcStem "lhs"
      lhsExists <- doesFileExist lhsFile
      when (lhsExists) (putStrLn ("Setup: delete " ++ hsFile ++ "\n  (generated from " ++ lhsFile ++ ")"))
      when (lhsExists) (removeFile hsFile)
      return ()

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


-- haddock is copied from Distribution.Simple,
-- and modified to work in this script.

haddock :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> HaddockFlags -> IO ()
haddock pkg_descr lbi hooks (HaddockFlags hoogle verbose) = do
    let pps = allSuffixHandlers hooks
    confHaddock <- do
      let programConf = withPrograms lbi
      mHaddock <- lookupProgram (programName haddockProgram) programConf
      maybe (die "haddock command not found") return mHaddock

    let tmpDir = combine (buildDir lbi) "tmp"
    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing True haddockPref
    preprocessSources pkg_descr lbi verbose pps

    setupMessage "Running Haddock for" pkg_descr

    let replaceLitExts = map (combine tmpDir . flip replaceExtension "hs")
    let mockAll bi = mapM_ (mockPP ["-D__HADDOCK__"] pkg_descr bi lbi tmpDir verbose)
    let showPkg     = showPackageId (package pkg_descr)
    let showDepPkgs = map showPackageId (packageDeps lbi)
    let outputFlag  = if hoogle then "--hoogle" else "--html"

    withLib pkg_descr () $ \lib -> do
        let bi = libBuildInfo lib
        inFiles <- getModulePaths bi (exposedModules lib ++ otherModules bi)
        mockAll bi inFiles
        let prologName = showPkg ++ "-haddock-prolog.txt"
        writeFile prologName (description pkg_descr ++ "\n")
        let outFiles = replaceLitExts inFiles
        let haddockFile = combine haddockPref (haddockName pkg_descr)
        -- FIX: replace w/ rawSystemProgramConf?
        rawSystemProgram verbose confHaddock
                ([outputFlag,
                  "--odir=" ++ haddockPref,
                  "--title=" ++ showPkg ++ ": " ++ synopsis pkg_descr,
                  "--package=" ++ showPkg,
                  "--dump-interface=" ++ haddockFile,
                  "--prologue=" ++ prologName]
                 -- ++ map ("--use-package=" ++) showDepPkgs
                 ++ map readInf (packageDeps lbi)
                 ++ programArgs confHaddock
                 ++ (if verbose > 4 then ["--verbose"] else [])
                 ++ outFiles
                 ++ map ("--hide=" ++) (otherModules bi)
                )
        removeFile prologName
    removeDirectoryRecursive tmpDir
  where 
        compilerTopDir = takeDirectory (takeDirectory (dropFileName (compilerPath (compiler lbi))))
        readInf pkgId = "--read-interface=http://www.haskell.org/ghc/docs/latest/html/libraries/"
          ++ pkgName pkgId ++ ","
          ++ joinPath [compilerTopDir, "html", "libraries", pkgName pkgId, pkgName pkgId ++ ".haddock"]
        mockPP inputArgs pkg_descr bi lbi pref verbose file
            = do let (filePref, fileName) = splitFileName file
                 let targetDir = combine pref filePref
                 let targetFile = combine targetDir fileName
                 let (targetFileNoext, targetFileExt) = splitExtension targetFile
                 createDirectoryIfMissing True targetDir
                 if (needsCpp pkg_descr)
                    then ppCpp' inputArgs bi lbi file targetFile verbose
                    else copyFile file targetFile >> return ExitSuccess
        needsCpp :: PackageDescription -> Bool
        needsCpp p =
           hasLibs p &&
           any (== CPP) (extensions $ libBuildInfo $ fromJust $ library p)



sameFolder path = return (fst (splitFileName path))
parentFolder path = canonicalizePath (fst (splitFileName path) ++ "/..")

makeConfig path libName libDir includeDir = do
  libDirs <- canonicalizePath (combine path libDir)
  includeDirs <- canonicalizePath (combine path includeDir)
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

#if mingw32_HOST_OS || mingw32_TARGET_OS
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
