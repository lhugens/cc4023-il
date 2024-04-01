{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cc4023_il (
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

bindir     = "/home/hugens/.cabal/bin"
libdir     = "/home/hugens/.cabal/lib/x86_64-linux-ghc-8.8.4/cc4023-il-0.1.0.0-inplace"
dynlibdir  = "/home/hugens/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/hugens/.cabal/share/x86_64-linux-ghc-8.8.4/cc4023-il-0.1.0.0"
libexecdir = "/home/hugens/.cabal/libexec/x86_64-linux-ghc-8.8.4/cc4023-il-0.1.0.0"
sysconfdir = "/home/hugens/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cc4023_il_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cc4023_il_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cc4023_il_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cc4023_il_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cc4023_il_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cc4023_il_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
