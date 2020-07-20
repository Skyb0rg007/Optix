{-# LANGUAGE CPP #-}

module System.DlOpen.Windows where

#if defined(mingw32_HOST_OS)

import           Foreign.Ptr      (castPtrToFunPtr)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Win32.DLL as Win

newtype DL = DL Win.HINSTANCE

dldefaultIO :: IO DL
dldefaultIO = DL <$> Win.getModuleHandle Nothing

dldefault :: DL
dldefault = unsafePerformIO dldefaultIO
{-# NOINLINE dldefault #-}

dlopen :: String -> IO DL
dlopen lib = DL <$> Win.loadLibrary lib

dlclose :: DL -> IO ()
dlclose (DL dl) = Win.freeLibrary dl

dlsym :: DL -> String -> IO (FunPtr a)
dlsym (DL dl) name = castPtrToFunPtr <$> Win.getProcAddress dl name

#endif

