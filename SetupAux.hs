{-# OPTIONS -cpp #-}
{-# LANGUAGE CPP #-}

module SetupAux where

import Control.Exception (bracket)
import Control.Monad (when, unless)
import Data.List (unionBy)
import Distribution.PackageDescription
  ( PackageDescription(..), BuildInfo(..), hasLibs )
import Foreign.Marshal.Alloc (allocaBytes)
import System.Directory
  ( getPermissions, setPermissions
  , createDirectory, removeDirectory
  , doesDirectoryExist, removeFile, getDirectoryContents )
import Distribution.PreProcess (PPSuffixHandler, knownSuffixHandlers)
import Distribution.Setup
  ( BuildFlags, InstallFlags(..), CopyFlags(..), CopyDest(..)
  , emptyRegisterFlags, regUser, regVerbose
  )
import Distribution.Simple ( UserHooks(..), hookedPreProcessors )
import Distribution.Simple.Build (build)
import Distribution.Simple.Install (install)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Register (writeInstalledConfig, register)
import Distribution.Simple.Utils (moduleToFilePath)
import System.FilePath (combine, isPathSeparator, pathSeparator)
import System.IO
  ( IOMode(..), hClose, openBinaryFile, hGetBuf, hPutBuf )
import System.IO.Error (try, ioError)

---------------------------------------------------------------------
-- Start of code copied verbatim from Distribution.Compat.FilePath,
-- because it's not exported.
pathParents :: FilePath -> [FilePath]
pathParents p =
    root'' : map ((++) root') (dropEmptyPath $ inits path')
    where
#if mingw32_HOST_OS || mingw32_TARGET_OS
       (root,path) = case break (== ':') p of
          (path,    "") -> ("",path)
          (root,_:path) -> (root++":",path)
#else
       (root,path) = ("",p)
#endif
       (root',root'',path') = case path of
         (c:path) | isPathSeparator c -> (root++[pathSeparator],root++[pathSeparator],path)
         _                            -> (root                 ,root++"."            ,path)

       dropEmptyPath ("":paths) = paths
       dropEmptyPath paths      = paths

       inits :: String -> [String]
       inits [] =  [""]
       inits cs =
         case pre of
           "."  -> inits suf
           ".." -> map (combine pre) (dropEmptyPath $ inits suf)
           _    -> "" : map (combine pre) (inits suf)
         where
           (pre,suf) = case break isPathSeparator cs of
              (pre,"")    -> (pre, "")
              (pre,_:suf) -> (pre,suf)
-- End of code copied verbatim from Distribution.Compat.FilePath.
---------------------------------------------------------------------


---------------------------------------------------------------------
-- Start of code copied verbatim from Distribution.Compat.Directory,
-- because it's not exported.
copyFileTimes :: FilePath -> FilePath -> IO ()
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
copyFileTimes src dest
   = do st <- getFileStatus src
        let atime = accessTime st
            mtime = modificationTime st
        setFileTimes dest atime mtime
#else
copyFileTimes src dest
    = return ()
#endif

copyPermissions :: FilePath -> FilePath -> IO ()
#if !(mingw32_HOST_OS || mingw32_TARGET_OS)
copyPermissions src dest
    = do srcStatus <- getFileStatus src
         setFileMode dest (fileMode srcStatus)
#else
copyPermissions src dest
    = getPermissions src >>= setPermissions dest
#endif

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest 
    | dest == src = fail "copyFile: source and destination are the same file"
    | otherwise = bracket (openBinaryFile src ReadMode) hClose $ \hSrc ->
                  bracket (openBinaryFile dest WriteMode) hClose $ \hDest ->
                  do allocaBytes bufSize $ \buffer -> copyContents hSrc hDest buffer
                     try (copyPermissions src dest)
                     try (copyFileTimes src dest)
                     return ()
  where bufSize = 1024
        copyContents hSrc hDest buffer
           = do count <- hGetBuf hSrc buffer bufSize
                when (count > 0) $ do hPutBuf hDest buffer count
                                      copyContents hSrc hDest buffer

createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
		         -> FilePath -- ^ The path to the directory you want to make
		         -> IO ()
createDirectoryIfMissing parents file = do
  b <- doesDirectoryExist file
  case (b,parents, file) of 
    (_,     _, "") -> return ()
    (True,  _,  _) -> return ()
    (_,  True,  _) -> mapM_ (createDirectoryIfMissing False) (tail (pathParents file))
    (_, False,  _) -> createDirectory file

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive startLoc = do
  cont <- getDirectoryContentsWithoutSpecial startLoc
  mapM_ (rm . combine startLoc) cont
  removeDirectory startLoc
  where
    rm :: FilePath -> IO ()
    rm f = do temp <- try (removeFile f)
              case temp of
                Left e  -> do isDir <- doesDirectoryExist f
                              -- If f is not a directory, re-throw the error
                              unless isDir $ ioError e
                              removeDirectoryRecursive f
                Right _ -> return ()

getDirectoryContentsWithoutSpecial :: FilePath -> IO [FilePath]
getDirectoryContentsWithoutSpecial =
   fmap (filter (not . flip elem [".", ".."])) . getDirectoryContents

-- End of code copied verbatim from Distribution.Compat.Directory.
---------------------------------------------------------------------


---------------------------------------------------------------------
-- Start of code copied verbatim from Distribution.Simple,
-- because it's not exported.
getModulePaths :: BuildInfo -> [String] -> IO [FilePath]
getModulePaths bi =
   fmap concat .
      mapM (flip (moduleToFilePath (hsSourceDirs bi)) ["hs", "lhs"])

defaultBuildHook :: PackageDescription -> LocalBuildInfo
    -> Maybe UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags = do
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False

defaultInstallHook :: PackageDescription -> LocalBuildInfo
    -> Maybe UserHooks ->InstallFlags -> IO ()
defaultInstallHook pkg_descr localbuildinfo _ (InstallFlags uInstFlag verbose) = do
  install pkg_descr localbuildinfo (CopyFlags NoCopyDest verbose)
  when (hasLibs pkg_descr) $
      register pkg_descr localbuildinfo 
           emptyRegisterFlags{ regUser=uInstFlag, regVerbose=verbose }

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
