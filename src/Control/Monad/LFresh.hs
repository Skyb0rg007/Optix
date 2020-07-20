
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.LFresh
    ( 
    -- * Monads with locally fresh capability
      MonadLFresh (..)
    -- * The LFreshT monad transformer
    , LFreshT
    , LFresh
    , runLFreshT
    , runLFresh
    , mapLFreshT
    , mapLFresh
    ) where

import           Control.Monad.Trans.Class         (MonadTrans (lift))
import           Control.Monad.Trans.Control       (MonadTransControl (..))
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
import           Data.HashSet                      (HashSet)
import qualified Data.HashSet                      as HashSet
import           Data.Text                         (Text)

import           Control.Monad.Trans.LFresh hiding (avoid, lfresh)
import qualified Control.Monad.Trans.LFresh as LFresh
import           Control.Monad.Trans.Warn   (WarnT)

class Monad m => MonadLFresh m where
    lfresh :: Text -> m Text
    avoid :: Foldable f => f Text -> m a -> m a

    default lfresh :: (MonadLFresh n, MonadTrans t, m ~ t n) => Text -> m Text
    lfresh = lift . lfresh
    default avoid :: (MonadLFresh n, MonadTransControl t, m ~ t n, Foldable f) => f Text -> m a -> m a
    avoid avoidSet action = liftWith (\run -> avoid avoidSet (run action)) >>= restoreT . pure

instance Monad m => MonadLFresh (LFreshT m) where
    lfresh = LFresh.lfresh
    avoid = LFresh.avoid

instance MonadLFresh m             => MonadLFresh (Except.ExceptT e m)
instance MonadLFresh m             => MonadLFresh (Identity.IdentityT m)
instance MonadLFresh m             => MonadLFresh (Maybe.MaybeT m)
instance MonadLFresh m             => MonadLFresh (Reader.ReaderT r m)
instance (Monoid w, MonadLFresh m) => MonadLFresh (LazyRWS.RWST r w s m)
instance (Monoid w, MonadLFresh m) => MonadLFresh (StrictRWS.RWST r w s m)
instance MonadLFresh m             => MonadLFresh (LazyState.StateT s m)
instance MonadLFresh m             => MonadLFresh (StrictState.StateT s m)
instance (Monoid w, MonadLFresh m) => MonadLFresh (LazyWriter.WriterT w m)
instance (Monoid w, MonadLFresh m) => MonadLFresh (StrictWriter.WriterT w m)

instance MonadLFresh m => MonadLFresh (WarnT w e m)

