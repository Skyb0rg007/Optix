{-# LANGUAGE CPP #-}

module System.DlOpen
    (
#if defined(mingw32_HOST_OS)
      module System.DlOpen.Windows
#else
      module System.DlOpen.Posix
#endif
    ) where

#if defined(mingw32_HOST_OS)
import System.DlOpen.Windows
#else
import System.DlOpen.Posix
#endif

