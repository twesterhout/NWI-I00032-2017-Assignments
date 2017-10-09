{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Exercise4 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tom/.cabal/bin"
libdir     = "/home/tom/.cabal/lib/x86_64-linux-ghc-8.0.2/Exercise4-0.1.0.0-ChRz9413F50KxoOEzWkw50"
dynlibdir  = "/home/tom/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/tom/.cabal/share/x86_64-linux-ghc-8.0.2/Exercise4-0.1.0.0"
libexecdir = "/home/tom/.cabal/libexec"
sysconfdir = "/home/tom/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Exercise4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Exercise4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Exercise4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Exercise4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Exercise4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Exercise4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
