{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.Optix.Core.Syntax where

import qualified Bound
import qualified Bound.Name
import qualified Bound.Scope                  as Bound
import           Control.DeepSeq              (NFData)
import           Control.Monad                (ap)
import           Data.Binary                  (Binary (get, put))
import           Data.Bytes.Serial            (Serial, Serial1)
import           Data.Data                    (Data)
import           Data.Deriving                (deriveEq1, deriveOrd1,
                                               deriveRead1, deriveShow1)
import           Data.Functor.Classes         (compare1, eq1, readsPrec1,
                                               showsPrec1)
import           Data.Hashable                (Hashable)
import           Data.Hashable.Lifted         (Hashable1)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Int                     (Int32)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import           Data.List (elemIndex)
import           Data.Maybe                   (listToMaybe, fromMaybe)
import qualified Data.SCargot                 as SCargot
import qualified Data.SCargot.Repr.WellFormed as SCargot
import           Data.Text                    (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic, Generic1)

import           Language.Optix.Utils.Located
import           Language.Optix.Utils.Pretty
import           Language.Optix.Utils.Fresh
import           Language.Optix.Utils.Orphans ()

-- * Definitions

newtype Program = Program
    { _programBody :: Exp
    }
    deriving (Generic, Data, Typeable, NFData, Binary, Hashable)

type ExpScope b a = Bound.Scope (Bound.Name.Name Text b) Exp_ a

type Exp = Exp_ Text
data Exp_ a
    = ExpVar !a
    | ExpApp !(Exp_ a) !(Exp_ a)
    | ExpInt !Int32
    | ExpBool !Bool
    | ExpUnit
    | ExpIf !(Exp_ a) !(Exp_ a) !(Exp_ a)
    | ExpFn !(ExpScope () a)
    | ExpLet !(Exp_ a) !(ExpScope () a)
    | ExpLetRec ![ExpScope Int a] !(ExpScope Int a)
    | ExpRecord !(HashMap Text (Exp_ a))
    | ExpGet !(Exp_ a) !Text
    | ExpConstraint !(Exp_ a) !Ty
    | ExpPrim !Text !Ty
    | ExpLocated !(Located (Exp_ a))
    deriving (Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable, NFData, Serial1, Serial, Binary, Hashable1, Hashable)

instance Applicative Exp_ where
    pure = ExpVar
    (<*>) = ap

instance Monad Exp_ where
    return = pure
    ExpVar a          >>= f = f a
    ExpApp a b        >>= f = ExpApp (a >>= f) (b >>= f)
    ExpInt n          >>= _ = ExpInt n
    ExpBool b         >>= _ = ExpBool b
    ExpUnit           >>= _ = ExpUnit
    ExpIf e1 e2 e3    >>= f = ExpIf (e1 >>= f) (e2 >>= f) (e3 >>= f)
    ExpFn s           >>= f = ExpFn (s Bound.>>>= f)
    ExpLet e1 e2      >>= f = ExpLet (e1 >>= f) (e2 Bound.>>>= f)
    ExpLetRec e1 e2   >>= f = ExpLetRec (map (Bound.>>>= f) e1) (e2 Bound.>>>= f)
    ExpRecord r       >>= f = ExpRecord (fmap (>>= f) r)
    ExpGet e x        >>= f = ExpGet (e >>= f) x
    ExpConstraint e t >>= f = ExpConstraint (e >>= f) t
    ExpPrim name ty   >>= _ = ExpPrim name ty
    ExpLocated (At s e) >>= f = ExpLocated (At s (e >>= f))

type TyScope a = Bound.Scope (Bound.Name.Name Text ()) Ty_ a

type Ty = Ty_ Text
data Ty_ a
    = TyVar !a
    | TyArrow !(Ty_ a) !(Ty_ a)
    | TyRecord !(HashMap Text (Ty_ a))
    | TyInt
    | TyBool
    | TyUnit
    | TyTop
    | TyBot
    | TyMeet !(Ty_ a) !(Ty_ a)
    | TyJoin !(Ty_ a) !(Ty_ a)
    | TyRecursive !(TyScope a)
    | TyLocated !(Located (Ty_ a))
    deriving (Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable, NFData, Serial1, Serial, Binary, Hashable1, Hashable)

instance Applicative Ty_ where
    pure = TyVar
    (<*>) = ap

instance Monad Ty_ where
    return = pure
    TyVar a       >>= f = f a
    TyArrow a b   >>= f = TyArrow (a >>= f) (b >>= f)
    TyRecord r    >>= f = TyRecord (fmap (>>= f) r)
    TyInt         >>= _ = TyInt
    TyBool        >>= _ = TyBool
    TyUnit        >>= _ = TyUnit
    TyTop         >>= _ = TyTop
    TyBot         >>= _ = TyBot
    TyMeet a b    >>= f = TyMeet (a >>= f) (b >>= f)
    TyJoin a b    >>= f = TyMeet (a >>= f) (b >>= f)
    TyRecursive t >>= f = TyRecursive (t Bound.>>>= f)

-- * Template Haskell

deriveEq1 ''Exp_
deriveOrd1 ''Exp_
deriveRead1 ''Exp_
deriveShow1 ''Exp_
deriveEq1 ''Ty_
deriveOrd1 ''Ty_
deriveRead1 ''Ty_
deriveShow1 ''Ty_

-- * Instances

instance Eq a => Eq (Exp_ a) where (==) = eq1
instance Ord a => Ord (Exp_ a) where compare = compare1
instance Read a => Read (Exp_ a) where readsPrec = readsPrec1
instance Show a => Show (Exp_ a) where showsPrec = showsPrec1
instance Eq a => Eq (Ty_ a) where (==) = eq1
instance Ord a => Ord (Ty_ a) where compare = compare1
instance Read a => Read (Ty_ a) where readsPrec = readsPrec1
instance Show a => Show (Ty_ a) where showsPrec = showsPrec1

deriving instance Show Program
deriving instance Eq Program
deriving instance Ord Program
deriving instance Read Program

-- * Pretty

data Style
    = StyleString
    | StyleInt
    | StyleKeyword
    | StyleTyVar
    | StyleVar
    | StyleType
    | StyleLabel
    | StyleBool
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

instance PrettyPrec Program Style where
    prettyPrec d = prettyPrec d . _programBody

instance PrettyPrec (Exp_ Text) Style where
    prettyPrec d = flip runFresh mempty . pp d
        where
            pp :: Int -> Exp -> Fresh (Doc Style)
            pp d = \case
                ExpVar x -> pure $ annotate StyleVar (pretty x)
                ExpApp e1 e2 -> do
                    e1' <- pp 1 e1
                    e2' <- pp 2 e2
                    pure $ parenIf (d > 1) $ e1' <+> e2'
                ExpInt n -> pure $ annotate StyleInt (pretty n)
                ExpBool True -> pure $ annotate StyleBool "true"
                ExpBool False -> pure $ annotate StyleBool "false"
                ExpUnit -> pure $ annotate StyleKeyword "()"
                ExpIf e1 e2 e3 -> do
                    e1' <- pp 0 e1
                    e2' <- pp 0 e2
                    e3' <- pp 0 e3
                    pure $
                        vsep
                        [ annotate StyleKeyword "if"
                        , indent 4 $
                            vsep
                            [ annotate StyleKeyword "then" <+> e2'
                            , annotate StyleKeyword "else" <+> e3'
                            ]
                        ]
                ExpFn body -> do
                    let x = fromMaybe "_" $ listToMaybe $ map Bound.Name.name (Bound.bindings body)
                    x' <- fresh x
                    body' <- pp 0 $ Bound.Name.instantiate1Name (ExpVar x') body
                    pure $ parenIf (d > 0) $
                        annotate StyleKeyword "fn"
                        <+> annotate StyleVar (pretty x')
                        <+> annotate StyleKeyword "=>"
                        <+> body'
                ExpLet e1 body -> do
                    e1' <- pp 0 e1
                    let x = fromMaybe "_" $ listToMaybe $ map Bound.Name.name (Bound.bindings body)
                    x' <- fresh x
                    let e2 = Bound.Name.instantiate1Name (ExpVar x') body
                    e2' <- avoid [x' | x' /= "_"] $ pp 0 e2
                    pure $
                        vsep
                        [ hsep [ annotate StyleKeyword "let"
                               , annotate StyleVar (pretty x')
                               , annotate StyleKeyword "="
                               , nest 4 e1'
                               , annotate StyleKeyword "in"
                               ]
                        , e2'
                        ]
                ExpLetRec binds body -> do
                    let nameOf bind = fromMaybe "_" $ listToMaybe $ map Bound.Name.name (Bound.bindings bind)
                        names = map nameOf binds
                    names' <- traverse fresh names
                    let inst = Bound.Name.instantiateName (map ExpVar names' !!)
                    binds' <- traverse (avoid (filter (/= "_") names') . pp 0 . inst) binds
                    body' <- pp 0 (inst body)
                    pure $
                        vsep
                        [ annotate StyleKeyword "letrec"
                        , indent 4 $
                            vsep $ flip map (zip names' binds') $ \(nm, e) ->
                                annotate StyleVar (pretty nm) <+> annotate StyleKeyword "=" <+> nest 4 e
                        , annotate StyleKeyword "in"
                        , body'
                        ]
                ExpRecord r ->
                    let f :: (Text, Exp) -> Fresh (Doc Style)
                        f (k, v) = do
                            v' <- pp 0 v
                            pure $ annotate StyleLabel (pretty k) <+> annotate StyleKeyword "=" <+> v'
                     in encloseSep "{ " " }" ", " <$> traverse f (HashMap.toList r) 
                ExpGet e x -> do
                    e' <- pp 2 e 
                    pure $ e' <> annotate StyleKeyword "." <> annotate StyleLabel (pretty x)
                ExpConstraint e t -> do
                    e' <- pp 0 e
                    let t' = prettyPrec 0 t
                    pure $ parenIf (d > 0) $ e' <+> annotate StyleKeyword ":" <+> t'
                ExpPrim x t -> do
                    let t' = prettyPrec 0 t
                    pure $ parenIf (d > 0) $
                        annotate StyleKeyword "prim" <+> pretty x <+> annotate StyleKeyword ":" <+> t'
                ExpLocated (At _ x) -> pp d x

instance Pretty a => PrettyPrec (Ty_ a) Style where
    prettyPrec d = \case
        _ -> "<type>"

