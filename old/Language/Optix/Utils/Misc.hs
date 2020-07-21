
module Language.Optix.Utils.Misc
    ( foldMapM
    ) where

import           Data.Foldable (foldlM)

-- Monadic 'foldMap'
foldMapM
    :: (Monad m, Monoid w, Foldable t)
    => (a -> m w)
    -> t a
    -> m w
foldMapM f = foldlM (\acc a -> fmap (mappend acc) (f a)) mempty

