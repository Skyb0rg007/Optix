{-# LANGUAGE CPP #-}

module System.DlOpen.Windows where

#if defined(mingw32_HOST_OS)

import           Foreign.Ptr
import qualified System.Win32.DLL as Win

newtype DL = DL Win.HINSTANCE

dldefault :: DL
dldefault = DL 

dlopen :: String -> IO DL
dlopen lib = DL <$> Win.loadLibrary lib

dlclose :: DL -> IO ()
dlclose (DL dl) = Win.freeLibrary dl

dlsym :: DL -> String -> IO (FunPtr a)
dlsym (DL dl) name = castPtrToFunPtr <$> Win.getProcAddress dl name

#endif

