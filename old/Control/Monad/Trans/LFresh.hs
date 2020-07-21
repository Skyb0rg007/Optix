
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Trans.LFresh
    ( 
    -- * LFresh monad
      LFresh
    , runLFresh
    , mapLFresh
    -- * LFresh monad transformer
    , LFreshT (..)
    , runLFreshT
    , mapLFreshT
    -- * LFresh operations
    , lfresh
    , avoid
    -- * Lifting other operations
    , liftCallCC
    , liftCatch
    ) where

import           Control.Monad.Base
import           Control.Monad.Cont          (MonadCont (..))
import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Morph         (MFunctor, MMonad)
import           Control.Monad.Reader
import           Control.Monad.Signatures    (CallCC, Catch)
import           Control.Monad.State         (MonadState (..))
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Reader  as Reader (liftCallCC, liftCatch)
import           Control.Monad.Writer        (MonadWriter (..))
import           Data.Foldable               (find, foldl')
import           Data.Functor.Identity       (Identity (..))
import           Data.Hashable               (Hashable)
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HashSet
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text (pack)

type LFresh = LFreshT Identity

runLFresh :: LFresh a -> HashSet Text -> a
runLFresh avoidSet = runIdentity . runLFreshT avoidSet

mapLFresh :: (a -> b) -> LFresh a -> LFresh b
mapLFresh f = mapLFreshT (Identity . f . runIdentity)

--

newtype LFreshT m a = LFreshT { unLFreshT :: ReaderT (HashSet Text) m a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadFix
        , MonadIO
        , MonadTrans
        , MonadTransControl
        , MFunctor
        , MMonad
        )

instance MonadBase IO m => MonadBase IO (LFreshT m) where
    liftBase = lift . liftBase

instance MonadBaseControl IO m => MonadBaseControl IO (LFreshT m) where
    type StM (LFreshT m) a = ComposeSt LFreshT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

runLFreshT :: LFreshT m a -> HashSet Text -> m a
runLFreshT = runReaderT . unLFreshT

mapLFreshT :: (m a -> n b) -> LFreshT m a -> LFreshT n b
mapLFreshT f = LFreshT . mapReaderT f . unLFreshT

--

lfresh :: Monad m => Text -> LFreshT m Text
lfresh name = LFreshT $ do
    avoidSet <- ask
    let names :: [Text]
        names = name : map (\i -> name <> Text.pack (show i)) [0..]
        valid :: Text -> Bool
        valid x = not (x `HashSet.member` avoidSet)
    pure $ fromJust $ find valid names

avoid :: (Foldable f, Monad m) => f Text -> LFreshT m a -> LFreshT m a
avoid names = LFreshT . local (HashSet.union (fromList names)) . unLFreshT
    where
        fromList :: (Eq a, Hashable a, Foldable f) => f a -> HashSet a
        fromList = foldl' (flip HashSet.insert) HashSet.empty

--

liftCatch :: Catch e m a -> Catch e (LFreshT m) a
liftCatch catch action handle = LFreshT $ Reader.liftCatch catch (unLFreshT action) (unLFreshT . handle)

liftCallCC :: CallCC m a b -> CallCC (LFreshT m) a b
liftCallCC callCC f = LFreshT $ Reader.liftCallCC callCC (unLFreshT . f . fmap LFreshT)

-- * Mtl instances

instance MonadReader r m => MonadReader r (LFreshT m) where
    ask = lift ask
    local = mapLFreshT . local
    reader = lift . reader

instance MonadError e' m => MonadError e' (LFreshT m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

instance MonadState s m => MonadState s (LFreshT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadWriter w' m => MonadWriter w' (LFreshT m) where
    writer = lift . writer
    tell = lift . tell
    listen = mapLFreshT listen
    pass = mapLFreshT pass

instance MonadCont m => MonadCont (LFreshT m) where
    callCC = liftCallCC callCC
