
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Optix.Utils.Orphans () where

import           Data.Binary         (Binary (get, put))
import           Data.Hashable       (Hashable (..))
import           Data.Hashable.Lifted (Hashable1 (..), hashWithSalt1)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HashMap k v) where
    get = fmap HashMap.fromList get
    {-# INLINE get #-}
    put = put . HashMap.toList
    {-# INLINE put #-}

instance (Hashable a, Eq a, Binary a) => Binary (HashSet a) where
    get = fmap HashSet.fromList get
    {-# INLINE get #-}
    put = put . HashSet.toList
    {-# INLINE put #-}

instance Hashable a => Hashable (IntMap a) where
    hashWithSalt = hashWithSalt1

instance Hashable1 IntMap where
    liftHashWithSalt h s m = liftHashWithSalt (liftHashWithSalt h) s (IntMap.toList m)

