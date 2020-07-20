
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

-- | Implements MonadBase, MonadBaseControl, MonadTransControl for WriterT
-- Cannot implement MFunctor or MMonad without access to internal representation

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Control.Monad.Trans.Writer.CPS.Extras
    ( liftPass
    , liftListen
    ) where

import           Control.Monad.Base
import           Control.Monad.Morph            (MFunctor (hoist))
import           Control.Monad.Signatures       (Listen, Pass)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Writer.CPS
import           Control.Monad.Writer.CPS       ()
import           Unsafe.Coerce                  (unsafeCoerce)

-- Needed because Control.Monad.Trans.Writer.CPS doesn't expose internal rep.
-- This is safe since 'WriterT' is a newtype.
-- 'Data.Coerce.coerce' can't be used since the newtype constructor isn't exported.
-- Used to implement 'MFunctor' since 'writerT' requires a 'Functor' constraint.
pattern WriterT :: (w -> m (a, w)) -> WriterT w m a
pattern WriterT x <- (unsafeCoerce -> x)
    where WriterT x = unsafeCoerce x
{-# COMPLETE WriterT #-}

instance (Monoid w, MonadBase b m) => MonadBase b (WriterT w m) where
    liftBase = liftBaseDefault

instance (Monoid w, MonadBaseControl b m) => MonadBaseControl b (WriterT w m) where
    type StM (WriterT w m) a = ComposeSt (WriterT w) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance Monoid w => MonadTransControl (WriterT w) where
    type StT (WriterT w) a = (a, w)
    liftWith f = writerT $ fmap (,mempty) (f runWriterT)
    restoreT = writerT

instance MFunctor (WriterT w) where
    hoist nat (WriterT m) = WriterT $ \w -> nat (m w)

rotate :: ((a, b), c) -> ((a, c), b)
rotate ((a, b), c) = ((a, c), b)

-- | Lift 'pass' through 'WriterT'
liftPass
    :: (Monad m, Monoid w)
    => Pass w' m (a, w)
    -> Pass w' (WriterT w m) a
liftPass f = mapWriterT (f . fmap rotate)

-- | Lift 'listen' through 'WriterT'
liftListen
    :: (Monad m, Monoid w)
    => Listen w' m (a, w)
    -> Listen w' (WriterT w m) a
liftListen f = mapWriterT (fmap rotate . f)

