{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_datools (
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

bindir     = "/Users/aharol/Workspace/haskell/datools/.stack-work/install/x86_64-osx/lts-8.0/8.0.2/bin"
libdir     = "/Users/aharol/Workspace/haskell/datools/.stack-work/install/x86_64-osx/lts-8.0/8.0.2/lib/x86_64-osx-ghc-8.0.2/datools-0.1.0.0-Liiqe4AOY4e5U6XV99USYi"
dynlibdir  = "/Users/aharol/Workspace/haskell/datools/.stack-work/install/x86_64-osx/lts-8.0/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/aharol/Workspace/haskell/datools/.stack-work/install/x86_64-osx/lts-8.0/8.0.2/share/x86_64-osx-ghc-8.0.2/datools-0.1.0.0"
libexecdir = "/Users/aharol/Workspace/haskell/datools/.stack-work/install/x86_64-osx/lts-8.0/8.0.2/libexec"
sysconfdir = "/Users/aharol/Workspace/haskell/datools/.stack-work/install/x86_64-osx/lts-8.0/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "datools_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "datools_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "datools_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "datools_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "datools_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "datools_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
