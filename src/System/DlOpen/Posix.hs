{-# LANGUAGE CPP #-}

module System.DlOpen.Posix where

#if !defined(mingw32_HOST_OS)

import           Foreign.Ptr
import qualified System.Posix.DynamicLinker as Posix

newtype DL = DL Posix.DL

dldefault :: DL
dldefault = DL Posix.Default

dlopen :: String -> IO DL
dlopen lib = DL <$> Posix.dlopen lib [Posix.RTLD_NOW, Posix.RTLD_GLOBAL]

dlclose :: DL -> IO ()
dlclose (DL dl) = Posix.dlclose dl

dlsym :: DL -> String -> IO (FunPtr a)
dlsym (DL dl) = Posix.dlsym dl

#endif
