
-- Direct translation of 'reachability.rs'

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Language.Optix.Typecheck.Reachability
    ( ID (..)
    , MonadReachability (addNode, addEdge)
    , ReachabilityT
    , runReachabilityT
    ) where

import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Data.Bifunctor             (bimap, second)
import           Data.Foldable              (toList)
import           Data.Set.Ordered           (OSet)
import qualified Data.Set.Ordered           as OSet

-- * Reachability

newtype ID = ID Int
    deriving (Eq, Ord)

class Monad m => MonadReachability m where
    -- Construct a new node
    addNode :: m ID
    -- Connects the two nodes, returning all the newly added edges
    -- Usage: addEdge lhs rhs <--> lhs <: rhs
    addEdge :: ID -> ID -> m [(ID, ID)]

--

data R = R
    { upsets   :: ![OSet Int]
    , downsets :: ![OSet Int]
    }

newtype ReachabilityT m a = ReachabilityT (StateT R m a)
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadTrans
                     )

instance MonadReachability m => MonadReachability (StateT s m) where
    addNode = lift addNode
    addEdge lhs rhs = lift (addEdge lhs rhs)

instance MonadReachability m => MonadReachability (ExceptT s m) where
    addNode = lift addNode
    addEdge lhs rhs = lift (addEdge lhs rhs)

runReachabilityT :: Monad m => ReachabilityT m a -> m a
runReachabilityT (ReachabilityT m) = m `evalStateT` R [] []

instance Monad m => MonadReachability (ReachabilityT m) where
    addNode = ReachabilityT $ do
        R { upsets = up, downsets = down } <- get
        let i = length up
        put $ R { upsets = up ++ [OSet.empty], downsets = down ++ [OSet.empty] }
        pure (ID i)
    
    addEdge (ID lhs) (ID rhs) = ReachabilityT $
        let -- From the Math.FFT.Base package
            adjustAt :: (a -> a) -> Int -> [a] -> [a]
            adjustAt f i = uncurry (++) . second (\(x:xs) -> f x : xs) . splitAt i
            loop :: Monad m => [(Int, Int)] -> [(Int, Int)] -> StateT R m [(ID, ID)]
            loop [] out = pure $ map (bimap ID ID) out
            loop ((lhs, rhs):work) out = do
                down <- gets downsets
                if rhs `OSet.member` (down !! lhs)
                   then loop work out
                   else do
                       modify' $ \s -> s
                           { upsets = adjustAt (OSet.|> lhs) rhs (upsets s)
                           , downsets = adjustAt (OSet.|> rhs) lhs (downsets s)
                           }
                       R { upsets = u, downsets = d } <- get
                       let moreWork = map (,rhs) (toList (u !! lhs))
                                   ++ map (lhs,) (toList (d !! rhs))
                       loop (work ++ moreWork) (out ++ [(lhs, rhs)])
         in loop [(lhs, rhs)] []

