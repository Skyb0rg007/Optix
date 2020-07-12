
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Optix.Utils.Orphans () where

import           Data.Binary         (Binary (get, put))
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet

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

