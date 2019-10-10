{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Inventory (
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

bindir     = "/Users/felicienfotiomanfo/Real-Projects/Inventory/.stack-work/install/x86_64-osx/a088cff8f9f00240c0ba66afa166bfc92fbf40efb4a048999c17a13468295ee4/8.6.5/bin"
libdir     = "/Users/felicienfotiomanfo/Real-Projects/Inventory/.stack-work/install/x86_64-osx/a088cff8f9f00240c0ba66afa166bfc92fbf40efb4a048999c17a13468295ee4/8.6.5/lib/x86_64-osx-ghc-8.6.5/Inventory-0.1.0.0-l2nepkdPNg7y4qoQBMvil-Inventory-exe"
dynlibdir  = "/Users/felicienfotiomanfo/Real-Projects/Inventory/.stack-work/install/x86_64-osx/a088cff8f9f00240c0ba66afa166bfc92fbf40efb4a048999c17a13468295ee4/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/felicienfotiomanfo/Real-Projects/Inventory/.stack-work/install/x86_64-osx/a088cff8f9f00240c0ba66afa166bfc92fbf40efb4a048999c17a13468295ee4/8.6.5/share/x86_64-osx-ghc-8.6.5/Inventory-0.1.0.0"
libexecdir = "/Users/felicienfotiomanfo/Real-Projects/Inventory/.stack-work/install/x86_64-osx/a088cff8f9f00240c0ba66afa166bfc92fbf40efb4a048999c17a13468295ee4/8.6.5/libexec/x86_64-osx-ghc-8.6.5/Inventory-0.1.0.0"
sysconfdir = "/Users/felicienfotiomanfo/Real-Projects/Inventory/.stack-work/install/x86_64-osx/a088cff8f9f00240c0ba66afa166bfc92fbf40efb4a048999c17a13468295ee4/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Inventory_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Inventory_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Inventory_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Inventory_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Inventory_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Inventory_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
