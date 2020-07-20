-- Monad transformer that supports both warnings and errors
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Warn
    (
    -- * Monads with warnings and errors
      MonadWarn (..)
    -- * The WarnT monad transformer
    , WarnT
    , Warn
    , runWarnT
    , mapWarnT
    , runWarn
    , mapWarn
    ) where

import           Control.Monad.Trans.Class         (MonadTrans (lift))
import qualified Control.Monad.Trans.Except        as Except
import qualified Control.Monad.Trans.Identity      as Identity
import qualified Control.Monad.Trans.Maybe         as Maybe
import qualified Control.Monad.Trans.Reader        as Reader
import qualified Control.Monad.Trans.RWS.Lazy      as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict    as StrictRWS
import qualified Control.Monad.Trans.State.Lazy    as LazyState
import qualified Control.Monad.Trans.State.Strict  as StrictState
import qualified Control.Monad.Trans.Writer.Lazy   as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter

import           Control.Monad.Trans.LFresh (LFreshT)
import           Control.Monad.Trans.Warn   hiding (err, warn)
import qualified Control.Monad.Trans.Warn   as Warn

class Monad m => MonadWarn w e m | m -> w, m -> e where
    warn :: w -> m ()
    err  :: e -> m a

    default warn :: (MonadWarn w e n, MonadTrans t, m ~ t n) => w -> m ()
    warn = lift . warn
    default err :: (MonadWarn w e n, MonadTrans t, m ~ t n) => e -> m a
    err = lift . err

instance Monad m => MonadWarn w e (WarnT w e m) where
    warn = Warn.warn
    err = Warn.err

instance MonadWarn w e m              => MonadWarn w e (Except.ExceptT e' m)
instance MonadWarn w e m              => MonadWarn w e (Identity.IdentityT m)
instance MonadWarn w e m              => MonadWarn w e (Maybe.MaybeT m)
instance MonadWarn w e m              => MonadWarn w e (Reader.ReaderT r m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (LazyRWS.RWST r w' s m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (StrictRWS.RWST r w' s m)
instance MonadWarn w e m              => MonadWarn w e (LazyState.StateT s m)
instance MonadWarn w e m              => MonadWarn w e (StrictState.StateT s m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (LazyWriter.WriterT w' m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (StrictWriter.WriterT w' m)

instance MonadWarn w e m => MonadWarn w e (LFreshT m)
