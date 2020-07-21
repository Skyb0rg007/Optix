
-- Edward Kmett's 'bound' library with a simpler API

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Wextra #-}

module Language.Optix.Utils.Bound
    ( Scope
    , Var (..)
    , pattern Scope
    , closed
    , abstract
    , instantiate
    , scopeNames
    ) where

import           Control.Applicative       (Alternative ((<|>)))
import           Control.DeepSeq           (NFData (..), NFData1 (..), rnf1)
import           Control.Monad             (ap)
import           Control.Monad.Morph       (MFunctor (hoist))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Binary               (Binary)
import           Data.Functor.Classes
import           Data.Hashable             (Hashable (..))
import           Data.Hashable.Lifted      (Hashable1 (..), hashWithSalt1)
import           Data.IntMap               (IntMap)
import qualified Data.IntMap.Strict        as IntMap
import           Data.Text                 (Text)
import           GHC.Generics              (Generic, Generic1)
import           Text.Read                 (Read (readPrec), ReadPrec)

import           Language.Optix.Utils.Orphans ()

-- | Variable, either free or bound
data Var a
    = BVar !Int
    | FVar !a
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable,
          Generic, Generic1, NFData, NFData1, Binary, Hashable, Hashable1)

instance Applicative Var where
    pure = FVar
    (<*>) = ap

instance Monad Var where
    BVar n >>= _ = BVar n
    FVar x >>= f = f x

instance Show1 Var where
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Var a -> ShowS
    liftShowsPrec sp _ d (FVar x) = showParen (d > 10) $ showString "FVar " . sp 11 x
    liftShowsPrec _  _ d (BVar n) = showParen (d > 10) $ showString "BVar" . showsPrec 11 n

instance Read1 Var where
    liftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (Var a)
    liftReadPrec rp _ = readData $
            readUnaryWith rp "FVar" FVar
        <|> readUnaryWith readPrec "BVar" BVar

instance Eq1 Var where
    liftEq :: (a -> b -> Bool) -> Var a -> Var b -> Bool
    liftEq eq (FVar a) (FVar b) = a `eq` b
    liftEq _ (BVar n) (BVar m)  = n == m
    liftEq _ _ _                = False

instance Ord1 Var where
    liftCompare :: (a -> b -> Ordering) -> Var a -> Var b -> Ordering
    liftCompare cmp (FVar a) (FVar b) = a `cmp` b
    liftCompare _ (BVar n) (BVar m)   = n `compare` m
    liftCompare _ FVar{} BVar{}       = LT
    liftCompare _ BVar{} FVar{}       = GT

-- | Scope, optimizing 'lift'. Accessed via pattern synonyms
data Scope f a = Scope'
    { _scopeName :: !(IntMap Text)
    , _scopeVal  :: !(f (Var (f a)))
    }
    deriving (Functor, Foldable, Traversable, Generic, Generic1)

instance Monad f => Applicative (Scope f) where
    pure a = Scope' IntMap.empty (pure . pure . pure $ a)
    (<*>) = ap

-- | Use (>>=) for capture-avoiding substitution
instance Monad f => Monad (Scope f) where
    Scope' nm e >>= f = Scope' nm $ e >>= \case
        BVar n -> pure (BVar n)
        FVar x -> x >>= _scopeVal . f -- TODO: determine if this respects Monad laws

-- | Use to embed an expression into a scope
instance MonadTrans Scope where
    lift :: Monad m => m a -> Scope m a
    lift m = Scope' IntMap.empty (pure . pure $ m)

-- | Use to change the expression the scope is acting on
instance MFunctor Scope where
    hoist :: Monad m => (forall b. m b -> n b) -> Scope m a -> Scope n a
    hoist t (Scope' nm e) = Scope' nm $ t ((fmap . fmap) t e)

instance NFData1 f => NFData1 (Scope f) where
    liftRnf :: (a -> ()) -> Scope f a -> ()
    liftRnf r (Scope' nm e) = rnf nm `seq` (liftRnf . liftRnf . liftRnf) r e

instance (NFData1 f, NFData a) => NFData (Scope f a) where
    rnf :: Scope f a -> ()
    rnf = rnf1

instance Hashable1 f => Hashable1 (Scope f) where
    liftHashWithSalt h salt (Scope' nm e) =
        (liftHashWithSalt . liftHashWithSalt . liftHashWithSalt) h (hashWithSalt salt nm) e

instance (Hashable1 f, Hashable a) => Hashable (Scope f a) where hashWithSalt = hashWithSalt1

-- | No Binary1, so use a QuantifiedConstraint
deriving instance (Binary a, forall b. Binary b => Binary (f b)) => Binary (Scope f a)

instance (Monad f, Show1 f) => Show1 (Scope f) where
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Scope f a -> ShowS
    liftShowsPrec sp sl d (Scope nm e) = showParen (d > 10) $
        showString "Scope "
        . showsPrec 11 nm
        . showChar ' '
        . liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl) 11 e

instance (Monad f, Read1 f) => Read1 (Scope f) where
    liftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (Scope f a)
    liftReadPrec rp rl = readData $
        readBinaryWith readPrec (liftReadPrec (liftReadPrec rp rl) (liftReadListPrec rp rl)) "Scope" Scope

-- | Alpha-equality
instance (Monad f, Eq1 f) => Eq1 (Scope f) where
    liftEq :: (a -> b -> Bool) -> Scope f a -> Scope f b -> Bool
    liftEq eq (Scope _ e1) (Scope _ e2) = liftEq (liftEq eq) e1 e2

instance (Monad f, Ord1 f) => Ord1 (Scope f) where
    liftCompare :: (a -> b -> Ordering) -> Scope f a -> Scope f b -> Ordering
    liftCompare cmp (Scope _ e1) (Scope _ e2) = liftCompare (liftCompare cmp) e1 e2

instance (Monad f, Show1 f, Show a) => Show (Scope f a) where showsPrec = showsPrec1
instance (Monad f, Read1 f, Read a) => Read (Scope f a) where readPrec = readPrec1
instance (Monad f, Eq1 f, Eq a) => Eq (Scope f a) where (==) = eq1
instance (Monad f, Ord1 f, Ord a) => Ord (Scope f a) where compare = compare1

-- | Determine if the expression is closed, returning a witness if it is
closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)

-- | Pattern synonym for dealing with Scopes in DeBuijn-style
pattern Scope :: Monad f => IntMap Text -> f (Var a) -> Scope f a
pattern Scope nm s <- Scope' nm (toDeBruijn -> s)
    where
        Scope nm s = Scope' nm (fromDeBruijn s)
{-# COMPLETE Scope #-}

-- | Get the names of bound variables
scopeNames :: Scope f a -> IntMap Text
scopeNames (Scope' nm _) = nm

-- | Variable abstraction
abstract :: Monad f => IntMap Text -> (a -> Maybe Int) -> f a -> Scope f a
abstract nm f e = Scope' nm (fmap k e)
    where
        k x = case f x of
                Nothing -> FVar (pure x)
                Just z -> BVar z

-- | Variable instantiation
instantiate :: Monad f => (Int -> f a) -> Scope f a -> f a
instantiate k (Scope' _ e) = e >>= \case
    BVar n -> k n
    FVar x -> x

-- Convert the efficient representation to deBruijn-style
toDeBruijn :: Monad f => f (Var (f a)) -> f (Var a)
toDeBruijn e = e >>= \case
    FVar x -> FVar <$> x
    BVar n -> pure $ BVar n

-- Convert the deBruijn-style to the representation with efficient 'lift'
fromDeBruijn :: Monad f => f (Var a) -> f (Var (f a))
fromDeBruijn = (fmap . fmap) pure
