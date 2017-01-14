{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_reverse_interpreter (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/GDev/Desktop/CS4012 - Topics in Functional Programming/reverse-interpreter/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/bin"
libdir     = "/Users/GDev/Desktop/CS4012 - Topics in Functional Programming/reverse-interpreter/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/lib/x86_64-osx-ghc-8.0.1/reverse-interpreter-0.1.0.0"
datadir    = "/Users/GDev/Desktop/CS4012 - Topics in Functional Programming/reverse-interpreter/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/share/x86_64-osx-ghc-8.0.1/reverse-interpreter-0.1.0.0"
libexecdir = "/Users/GDev/Desktop/CS4012 - Topics in Functional Programming/reverse-interpreter/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/libexec"
sysconfdir = "/Users/GDev/Desktop/CS4012 - Topics in Functional Programming/reverse-interpreter/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "reverse_interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "reverse_interpreter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "reverse_interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "reverse_interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "reverse_interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
