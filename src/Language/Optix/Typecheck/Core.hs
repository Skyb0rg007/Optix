
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.Optix.Typecheck.Core where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable              (for_)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)

import           Language.Optix.Typecheck.Reachability
import           Language.Optix.Utils.Pretty (tshow)
import           Language.Optix.Utils.Misc (foldMapM)

newtype Value = Value Int
    deriving (Show)
newtype Use = Use Int
    deriving (Show)

-- Value head constructors
-- Corresponds to introduction rules
data VTypeHead
    = VBool
    | VUnit
    | VInt
    | VFunc !Use !Value
    | VRecord !(HashMap Text Value)
    deriving (Show)

-- Use-site head constructors
-- Corresponds to elimination rules
data UTypeHead
    = UBool
    | UUnit
    | UInt
    | UFunc !Value !Use
    | URecord !Text !Use
    deriving (Show)

class Monad m => MonadTypecheck m where
    -- lhs <: rhs
    flow :: Value -> Use -> m ()
    newVal :: VTypeHead -> m Value
    newUse :: UTypeHead -> m Use
    newVar :: m (Value, Use)

-- * Implementation

data TypeNode
    = TNVar
    | TNValue !VTypeHead
    | TNUse !UTypeHead
    deriving (Show)

newtype TypecheckT m a = TypecheckT (ExceptT Text (StateT [TypeNode] (ReachabilityT m)) a)
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadReachability
                     )

runTypecheckT :: Monad m => TypecheckT m a -> m (Either Text a)
runTypecheckT (TypecheckT m) = runReachabilityT $ runExceptT m `evalStateT` []

checkHeads :: MonadError Text m => VTypeHead -> UTypeHead -> m [(Value, Use)]
checkHeads lhs rhs = 
    case (lhs, rhs) of
      (VBool, UBool) -> pure []
      (VFunc a b, UFunc a' b') -> pure [(b, b'), (a', a)]
      (VRecord r, URecord name rhs') ->
          case HashMap.lookup name r of
            Nothing -> throwError $ "Missing field \"" <> name <> "\"!"
            Just lhs' -> pure [(lhs', rhs')]
      _ -> throwError $ "Unexpected types: " <> tshow lhs <> " <: " <> tshow rhs

instance Monad m => MonadTypecheck (TypecheckT m) where
    flow lhs rhs = TypecheckT $
        let loop :: Monad m => [(Value, Use)] -> ExceptT Text (StateT [TypeNode] (ReachabilityT m)) ()
            loop [] = pure ()
            loop ((Value lhs, Use rhs):pending) = do
                newEdges <- addEdge (ID lhs) (ID rhs)
                newPending <- flip foldMapM newEdges $ \(lhs', rhs') -> do
                    tyNodes <- get
                    case (tyNodes !! lhs, tyNodes !! rhs) of
                      (TNValue lhsHead, TNUse rhsHead) -> checkHeads lhsHead rhsHead
                      _ -> pure []
                loop (pending ++ newPending)
         in loop [(lhs, rhs)]
    newVal valType = TypecheckT $ do
        ID i <- addNode
        -- assert (length tyNodes == i)
        modify' $ \s -> s ++ [TNValue valType]
        pure $ Value i
    newUse useType = TypecheckT $ do
        ID i <- addNode
        -- assert (length tyNodes == i)
        modify' $ \s -> s ++ [TNUse useType]
        pure $ Use i
    newVar = TypecheckT $ do
        ID i <- addNode
        -- assert (length tyNodes == i)
        modify' $ \s -> s ++ [TNVar]
        pure (Value i, Use i)

