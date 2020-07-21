{-# LANGUAGE CPP #-}

module System.DlOpen
    ( DL
    , dlopen
    , dlclose
    , dldefault
    , dlsym
    ) where

#if defined(mingw32_HOST_OS)
import           System.DlOpen.Windows
#else
import           System.DlOpen.Posix
#endif

