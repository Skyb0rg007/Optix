
module Control.Monad.Trans.Except.Extras
    ( liftCatch
    ) where

import           Control.Monad.Signatures   (Catch)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)

-- | Lift 'catchE' through 'ExceptT'
liftCatch
    :: Catch e' m (Either e a)
    -> Catch e' (ExceptT e m) a
liftCatch catch action handle = ExceptT $ catch (runExceptT action) (runExceptT . handle)

