module Paths_GameOfLife (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/zetta/Code/life/.cabal-sandbox/bin"
libdir     = "/home/zetta/Code/life/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/GameOfLife-0.1.0.0"
datadir    = "/home/zetta/Code/life/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/GameOfLife-0.1.0.0"
libexecdir = "/home/zetta/Code/life/.cabal-sandbox/libexec"
sysconfdir = "/home/zetta/Code/life/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GameOfLife_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GameOfLife_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GameOfLife_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GameOfLife_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GameOfLife_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
