
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Optix.Typecheck
    ( typecheck
    ) where

import qualified Bound
import qualified Bound.Name
import           Data.Text (Text)
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Data.Functor.Identity

import           Language.Optix.Typecheck.Core
import           Language.Optix.Typecheck.Reachability
import           Language.Optix.Core.Syntax

typecheck :: Exp_ a -> Either Text Value
typecheck e = 
    case Bound.closed e of
      Nothing -> Left "Expression contains free variables!"
      Just e' -> runIdentity . runTypecheckT . tc $ e'
    where
        tc :: MonadTypecheck m => Exp_ Value -> m Value
        tc = \case
            ExpVar x -> pure x
            ExpApp e1 e2 -> do
                -- Γ ⊢ e₁ : τ₁
                -- Γ ⊢ e₂ : τ₂
                -- τ₁ <: τ₂ → α
                -- -------------
                -- Γ ⊢ e₁ e₂ : α
                t1 <- tc e1
                t2 <- tc e2
                (retTy, retBound) <- newVar
                bound <- newUse $ UFunc t2 retBound
                t1 `flow` bound
                pure retTy
            ExpInt {} -> newVal VInt
            ExpBool {} -> newVal VBool
            ExpUnit {} -> newVal VUnit
            ExpIf e1 e2 e3 -> do
                -- Γ ⊢ e₁ : τ₁
                -- Γ ⊢ e₂ : τ₂
                -- Γ ⊢ e₃ : τ₃
                -- τ₁ <: bool
                -- τ₂ <: α
                -- τ₃ <: α
                -- ------------
                -- Γ ⊢ if e₁ e₂ e₃ : α
                t1 <- tc e1
                t2 <- tc e2
                t3 <- tc e3
                boolUse <- newUse UBool
                t1 `flow` boolUse
                (merged, mergedUse) <- newVar
                t2 `flow` mergedUse
                t3 `flow` mergedUse
                pure merged
            ExpFn scope -> do
                -- Γ, x : τ₁ ⊢ e : τ₂
                -- -----------
                -- Γ ⊢ λx.e : τ₁ → τ₂
                (argTy, argTyUse) <- newVar
                bodyTy <- tc (Bound.Name.instantiate1Name (ExpVar argTy) scope)
                newVal $ VFunc argTyUse bodyTy
            ExpLet e1 e2 -> do
                -- Γ ⊢ e₁ : τ₁
                -- Γ, x : τ₁ ⊢ e₂ : τ₂
                -- --------------------
                -- Γ ⊢ let x = e₁ in e₂ : τ₂
                t1 <- tc e1
                tc (Bound.Name.instantiate1Name (ExpVar t1) e2)
            ExpLetRec {} -> error "letrec not implemented"
            ExpRecord r -> do
                r' <- traverse tc r
                newVal $ VRecord r'
            ExpGet e x -> do
                -- Γ ⊢ e : τ
                -- τ <: { x : τ' }
                -- --------------------
                -- Γ ⊢ e.x : τ'
                t <- tc e
                (fldTy, fldTyUse) <- newVar
                bound <- newUse $ URecord x fldTyUse
                t `flow` bound
                pure fldTy
            _ -> error "NYI"

