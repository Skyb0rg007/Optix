
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Control.Monad.Trans.Warn
    ( 
    -- * The Warn monad
      Warn
    , runWarn
    , mapWarn
    -- * The WarnT monad transformer
    , WarnT (..)
    , runWarnT
    , mapWarnT
    -- * Warn operations
    , warn
    , err
    -- * Lifting other operations
    , liftCallCC
    , liftCatch
    , liftPass
    , liftListen
    ) where

import           Control.Applicative                   (Alternative)
import           Control.Monad                         (MonadPlus)
import           Control.Monad.Base
import           Control.Monad.Cont.Class              (MonadCont (callCC))
import           Control.Monad.Except                  (MonadError (catchError, throwError))
import           Control.Monad.Fix                     (MonadFix)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Reader.Class            (MonadReader (ask, local, reader))
import           Control.Monad.Signatures              (CallCC, Catch, Listen, Pass)
import           Control.Monad.State.Class             (MonadState (get, put, state))
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Except            (ExceptT, runExceptT)
import qualified Control.Monad.Trans.Except            as Except
import qualified Control.Monad.Trans.Except.Extras     as Except
import           Control.Monad.Trans.Writer.CPS        (WriterT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS        as Writer
import qualified Control.Monad.Trans.Writer.CPS.Extras as Writer
import           Control.Monad.Writer.Class            (MonadWriter (..))
import           Data.Functor.Identity                 (Identity (..))
import           Data.Sequence                         (Seq)

-- * The Warn monad

type Warn w e = WarnT w e Identity

runWarn :: Warn w e a -> (Either e a, Seq w)
runWarn = runIdentity . runWarnT

mapWarn
    ::((Either e a, Seq w) -> (Either e a, Seq w))
    -> Warn w e a
    -> Warn w e a
mapWarn f = mapWarnT (Identity . f . runIdentity)

newtype WarnT w e m a = WarnT { unWarnT :: ExceptT e (WriterT (Seq w) m) a }
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadIO
        , MonadPlus
        , MonadFix
        )

instance MonadTrans (WarnT w e) where
    lift = WarnT . lift . lift

instance MonadTransControl (WarnT w e) where
    type StT (WarnT w e) a = StT (WriterT (Seq w)) (StT (ExceptT e) a)
    liftWith = defaultLiftWith2 WarnT unWarnT
    restoreT = defaultRestoreT2 WarnT

instance MonadBase IO m => MonadBase IO (WarnT w e m) where
    liftBase = lift . liftBase

instance MonadBaseControl IO m => MonadBaseControl IO (WarnT w e m) where
    type StM (WarnT w e m) a = ComposeSt (WarnT w e) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

mapWarnT
    :: Monad n
    => (m (Either e a, Seq w) -> n (Either e a, Seq w))
    -> WarnT w e m a
    -> WarnT w e n a
mapWarnT f = WarnT . (Except.mapExceptT . Writer.mapWriterT) f . unWarnT

runWarnT :: Monad m => WarnT w e m a -> m (Either e a, Seq w)
runWarnT = runWriterT . runExceptT . unWarnT

-- * Warn operations

warn :: Monad m => w -> WarnT w e m ()
warn = WarnT . lift . Writer.tell . pure

err :: Monad m => e -> WarnT w e m a
err = WarnT . Except.throwE

-- * Lifting other operations

liftListen
    :: (Monoid w, Monad m)
    => Listen w m (Either e a, Seq w')
    -> Listen w (WarnT w' e m) a
liftListen listen = WarnT . (Except.liftListen . Writer.liftListen) listen . unWarnT

liftPass
    :: (Monoid w, Monad m)
    => Pass w m (Either e a, Seq w')
    -> Pass w (WarnT w' e m) a
liftPass pass = WarnT . (Except.liftPass . Writer.liftPass) pass . unWarnT

liftCatch
    :: (Monad m)
    => Catch e m (Either e' a, Seq w)
    -> Catch e (WarnT w e' m) a
liftCatch catch m h = WarnT $ (Except.liftCatch . Writer.liftCatch) catch (unWarnT m) (unWarnT . h)

liftCallCC
    :: CallCC m (Either e a, Seq w) (Either e b, Seq w)
    -> CallCC (WarnT w e m) a b
liftCallCC callCC f = WarnT $ (Except.liftCallCC . Writer.liftCallCC) callCC $ \k -> unWarnT $ f (WarnT . k)

-- * Mtl instances

instance MonadReader r m => MonadReader r (WarnT w e m) where
    ask = lift ask
    local = mapWarnT . local
    reader = lift . reader

instance MonadError e' m => MonadError e' (WarnT w e m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

instance MonadState s m => MonadState s (WarnT w e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadWriter w' m => MonadWriter w' (WarnT w e m) where
    writer = lift . writer
    tell = lift . tell
    listen = liftListen listen
    pass = liftPass pass

instance MonadCont m => MonadCont (WarnT w e m) where
    callCC = liftCallCC callCC

