
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Warn where

import           Control.Applicative                   (Alternative)
import           Control.Monad                         (MonadPlus)
import           Control.Monad.Base
import           Control.Monad.Fix                     (MonadFix)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Except            as Except
import qualified Control.Monad.Trans.Identity          as Identity
import qualified Control.Monad.Trans.Maybe             as Maybe
import qualified Control.Monad.Trans.Reader            as Reader
import qualified Control.Monad.Trans.RWS.CPS           as CPSRWS
import qualified Control.Monad.Trans.RWS.Lazy          as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict        as StrictRWS
import qualified Control.Monad.Trans.State.Lazy        as LazyState
import qualified Control.Monad.Trans.State.Strict      as StrictState
import qualified Control.Monad.Trans.Writer.CPS        as CPSWriter
import qualified Control.Monad.Trans.Writer.CPS.Extras ()
import qualified Control.Monad.Trans.Writer.Lazy       as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict     as StrictWriter
import           Data.Functor.Identity                 (Identity (..))
import           Data.Sequence                         (Seq)

class Monad m => MonadWarn w e m | m -> w, m -> e where
    warn :: w -> m ()
    err  :: e -> m a

    default warn :: (MonadWarn w e n, MonadTrans t, m ~ t n) => w -> m ()
    warn = lift . warn
    default err :: (MonadWarn w e n, MonadTrans t, m ~ t n) => e -> m a
    err = lift . err

-- * Warn monad
type Warn w e = WarnT w e Identity

runWarn :: Warn w e a -> (Either e a, Seq w)
runWarn = runIdentity . runWarnT

mapWarn
    :: ((Either e a, Seq w) -> (Either e a, Seq w))
    -> Warn w e a
    -> Warn w e a
mapWarn f = mapWarnT (Identity . f . runIdentity)

-- * WarnT monad transformer

newtype WarnT w e m a = WarnT { unWarnT :: Except.ExceptT e (CPSWriter.WriterT (Seq w) m) a }
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
    type StT (WarnT w e) a = StT (CPSWriter.WriterT (Seq w)) (StT (Except.ExceptT e) a)
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
mapWarnT f = WarnT . (Except.mapExceptT . CPSWriter.mapWriterT) f . unWarnT

runWarnT :: WarnT w e m a -> m (Either e a, Seq w)
runWarnT = CPSWriter.runWriterT . Except.runExceptT . unWarnT

instance Monad m => MonadWarn w e (WarnT w e m) where
    warn = WarnT . lift . CPSWriter.tell . pure
    err = WarnT . Except.throwE

instance MonadWarn w e m              => MonadWarn w e (Except.ExceptT e' m)
instance MonadWarn w e m              => MonadWarn w e (Identity.IdentityT m)
instance MonadWarn w e m              => MonadWarn w e (Maybe.MaybeT m)
instance MonadWarn w e m              => MonadWarn w e (Reader.ReaderT r m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (LazyRWS.RWST r w' s m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (StrictRWS.RWST r w' s m)
instance (MonadWarn w e m) => MonadWarn w e (CPSRWS.RWST r w' s m)
instance MonadWarn w e m              => MonadWarn w e (LazyState.StateT s m)
instance MonadWarn w e m              => MonadWarn w e (StrictState.StateT s m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (LazyWriter.WriterT w' m)
instance (Monoid w', MonadWarn w e m) => MonadWarn w e (StrictWriter.WriterT w' m)
instance (MonadWarn w e m) => MonadWarn w e (CPSWriter.WriterT w' m)

