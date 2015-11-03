module Paths_arhelk_armenian (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lemarwin/code/arhelk/arhelk-armenian/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/bin"
libdir     = "/home/lemarwin/code/arhelk/arhelk-armenian/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/lib/x86_64-linux-ghc-7.10.2/arhelk-armenian-0.1.0.0-5l4tN5K8mLn1X8xpt1McF3"
datadir    = "/home/lemarwin/code/arhelk/arhelk-armenian/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/share/x86_64-linux-ghc-7.10.2/arhelk-armenian-0.1.0.0"
libexecdir = "/home/lemarwin/code/arhelk/arhelk-armenian/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/libexec"
sysconfdir = "/home/lemarwin/code/arhelk/arhelk-armenian/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arhelk_armenian_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arhelk_armenian_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "arhelk_armenian_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arhelk_armenian_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arhelk_armenian_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
