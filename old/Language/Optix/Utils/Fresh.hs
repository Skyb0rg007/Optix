
-- Implementation comes from
-- https://hackage.haskell.org/package/unbound-0.5.1.1/docs/src/Unbound.LocallyNameless.Fresh.html

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Language.Optix.Utils.Fresh
    ( MonadFresh (fresh, avoid)
    , FreshT
    , runFreshT
    , Fresh
    , runFresh
    ) where

import           Control.Monad.Reader  (ReaderT, ask, local, runReaderT)
import           Control.Monad.Trans   (MonadTrans)
import           Data.Foldable         (find, foldl')
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Hashable         (Hashable)
import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HashSet
import           Data.Maybe            (fromJust)
import           Data.Text             (Text)

import           Language.Optix.Utils.Pretty (tshow)

class Monad m => MonadFresh m where
    -- Get a fresh name
    fresh :: Text -> m Text
    -- Avoid the given names when running in the following scope
    avoid :: Foldable f => f Text -> m a -> m a

newtype FreshT m a = FreshT { unFreshT :: ReaderT (HashSet Text) m a }
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadTrans
                     )

runFreshT :: Monad m => FreshT m a -> HashSet Text -> m a
runFreshT = runReaderT . unFreshT

type Fresh a = FreshT Identity a

runFresh :: Fresh a -> HashSet Text -> a
runFresh used = runIdentity . runFreshT used

instance Monad m => MonadFresh (FreshT m) where
    fresh name = FreshT $ do
        used <- ask
        let names :: [Text]
            names = name : map (\i -> name <> tshow i) [0..]
            valid :: Text -> Bool
            valid x = not (x `HashSet.member` used)
        pure $ fromJust $ find valid names
    avoid names = FreshT . local (HashSet.union (fromList names)) . unFreshT
        where
            fromList :: (Eq a, Hashable a, Foldable f) => f a -> HashSet a
            fromList = foldl' (flip HashSet.insert) HashSet.empty

