{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bomberman (
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

bindir     = "/home/saketh0199/CSE230/Project/cse230_group32/.stack-work/install/x86_64-linux-tinfo6/5b02a977b3bfa4410b1c33734c232137102197cf2c85f7e6e46d54f200695212/8.10.4/bin"
libdir     = "/home/saketh0199/CSE230/Project/cse230_group32/.stack-work/install/x86_64-linux-tinfo6/5b02a977b3bfa4410b1c33734c232137102197cf2c85f7e6e46d54f200695212/8.10.4/lib/x86_64-linux-ghc-8.10.4/bomberman-0.1.0.0-EGZwZfCIIq4KTZWYgfPMde-bomberman"
dynlibdir  = "/home/saketh0199/CSE230/Project/cse230_group32/.stack-work/install/x86_64-linux-tinfo6/5b02a977b3bfa4410b1c33734c232137102197cf2c85f7e6e46d54f200695212/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/saketh0199/CSE230/Project/cse230_group32/.stack-work/install/x86_64-linux-tinfo6/5b02a977b3bfa4410b1c33734c232137102197cf2c85f7e6e46d54f200695212/8.10.4/share/x86_64-linux-ghc-8.10.4/bomberman-0.1.0.0"
libexecdir = "/home/saketh0199/CSE230/Project/cse230_group32/.stack-work/install/x86_64-linux-tinfo6/5b02a977b3bfa4410b1c33734c232137102197cf2c85f7e6e46d54f200695212/8.10.4/libexec/x86_64-linux-ghc-8.10.4/bomberman-0.1.0.0"
sysconfdir = "/home/saketh0199/CSE230/Project/cse230_group32/.stack-work/install/x86_64-linux-tinfo6/5b02a977b3bfa4410b1c33734c232137102197cf2c85f7e6e46d54f200695212/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bomberman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bomberman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bomberman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bomberman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bomberman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bomberman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
