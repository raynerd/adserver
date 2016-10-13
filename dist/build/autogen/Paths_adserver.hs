module Paths_adserver (
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/rayner/.cabal/bin"
libdir     = "/Users/rayner/.cabal/lib/x86_64-osx-ghc-7.10.3/adserver-0.1-BKRdiGA3NqV0YQ1NQZ785j"
datadir    = "/Users/rayner/.cabal/share/x86_64-osx-ghc-7.10.3/adserver-0.1"
libexecdir = "/Users/rayner/.cabal/libexec"
sysconfdir = "/Users/rayner/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "adserver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "adserver_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "adserver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adserver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adserver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
