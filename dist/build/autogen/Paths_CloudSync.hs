module Paths_CloudSync (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/user/.cabal/bin"
libdir     = "/home/user/.cabal/lib/i386-linux-ghc-7.4.2/CloudSync-0.0.1"
datadir    = "/home/user/.cabal/share/i386-linux-ghc-7.4.2/CloudSync-0.0.1"
libexecdir = "/home/user/.cabal/libexec"
sysconfdir = "/home/user/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CloudSync_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CloudSync_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CloudSync_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CloudSync_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CloudSync_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
