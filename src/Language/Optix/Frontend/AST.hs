{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Language.Optix.Frontend.AST where

import           Control.DeepSeq           (NFData)
import           Data.Binary               (Binary)
import           Data.Data                 (Data)
import           Data.Functor              ((<&>))
import           Data.Hashable             (Hashable)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Data.Scientific (Scientific)

import           Language.Optix.Utils.Located
import           Language.Optix.Utils.Pretty

-- Fixity type
data Fixity
    = InfixL (Maybe Int)
    | InfixR (Maybe Int)
    | Infix  (Maybe Int)
    | Nonfix
    deriving (Show, Eq, Ord, Read, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Whether the id is prefixed by 'op'
data Fixop = Op | NoOp
    deriving (Show, Eq, Ord, Read, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Constant
data Const
    = ConstBool Bool
    | ConstChar Char
    | ConstString Text
    | ConstInt Integer
    | ConstReal Scientific
    deriving (Show, Eq, Ord, Read, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Pattern
type Pat = Located Pat_
data Pat_
    = PatApp Text (Maybe Pat)         -- con (x, xs)
    | PatAs Fixop Text (Maybe Ty) Pat -- op x : τ as p
    | PatConst Const                  -- 12
    | PatConstraint Pat Ty            -- p : τ
    | PatFlatApp [Pat]                -- x :: xs
    | PatParen Pat                    -- (p)
    | PatRecord [Located (Text, PatItem)] -- { foo } | { foo = p }
    | PatVar Fixop Text               -- op x
    | PatWild                         -- _
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

data PatItem
    = PatItemField Pat
    | PatItemVid Text (Maybe Ty) (Maybe Pat) -- x [: τ] [as p]
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Types
type Ty = Located Ty_
data Ty_
    = TyCon Text [Ty]               -- TyCon "->" [TyVar "'a", TyVar "'b"] == 'a -> 'b
    | TyParen Ty                    -- TyParen τ == (τ)
    | TyRecord [Located (Text, Ty)] -- TyRecord [("foo", τ)] == { foo : τ }
    | TyVar Text                    -- TyVar "'a" == 'a
    | TyMeet Ty Ty
    | TyJoin Ty Ty
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Expressions
type Exp = Located Exp_
data Exp_
    = ExpAndAlso Exp Exp              -- e₁ andalso e₂
    | ExpApp Exp Exp                  -- e₁ e₂
    | ExpConst Const                  -- 12
    | ExpConstraint Exp Ty            -- e : τ
    | ExpFlatApp [Exp]                -- e1 ... en
    | ExpFn [Located (Pat, Exp)]      -- fn p1 => e1 | ... | pn => en
    | ExpIf Exp Exp Exp               -- if e₁ then e₂ else e₃
    | ExpLet [Dec] Exp                -- let d1 ... dn in e end
    | ExpOrElse Exp Exp               -- e₁ orelse e₂
    | ExpParen Exp                    -- (e)
    | ExpPrim Prim                    -- _prim "name" : τ;
    | ExpRecord [Located (Text, Exp)] -- { foo = e1, ..., bar = en }
    | ExpGet Exp Text                 -- e.x
    | ExpSequence [Exp]               -- (e₁; e₂; e₃)
    | ExpVar Fixop Text               -- op x
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Primitives
type Prim = Located Prim_
data Prim_ = Prim
    { _primName :: Text
    , _primType :: Ty
    }
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Declarations
type Dec = Located Dec_
data Dec_
    = DecDo Exp
    | DecFixity Fixity [Text]
    | DecFun [Located [FunClause]]
    | DecVal [ValClause]
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

type FunClause = Located FunClause_
data FunClause_ = FunClause
    { _funClausePats :: [Pat]
    , _funClauseRet  :: Maybe Ty
    , _funClauseBody :: Exp
    }
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

type ValClause = Located ValClause_
data ValClause_ = ValClause
    { _valClausePat :: Pat
    , _valClauseExp :: Exp
    }
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

-- | Program
newtype Program = Program
    { _programDecs :: [Dec]
    }
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

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
    | StyleChar
    | StyleCon
    deriving (Show, Eq, Ord, Read, Generic, Binary, Hashable, NFData, Data, Typeable)

instance PrettyPrec Const Style where
    prettyPrec _ = \case
        ConstBool True  -> annotate StyleBool "true"
        ConstBool False -> annotate StyleBool "true"
        ConstChar c     -> annotate StyleChar ("'" <> pretty c <> "'")
        ConstString s   -> annotate StyleString ("\"" <> pretty s <> "\"")
        ConstInt n      -> annotate StyleInt (pretty n)

instance PrettyPrec Pat_ Style where
    prettyPrec d = \case
        PatApp con Nothing -> annotate StyleCon (pretty con)
        PatApp con (Just arg) ->
            encloseSep "(" ")" " " $
                annotate StyleCon (pretty con) : map (prettyPrec 0) [arg]
        PatConst c -> prettyPrec d c
        PatFlatApp pats -> encloseSep "(" ")" " " $ map (prettyPrec 0) pats
        PatConstraint p t -> parenIf (d > 0) $
            prettyPrec 0 p <+> ":" <+> prettyPrec 0 t
        PatParen p -> parens $ prettyPrec 0 p
        PatRecord [] -> annotate StyleKeyword "()"
        PatRecord r -> encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " $
            r <&> \(At _ (k, v)) ->
                annotate StyleLabel (pretty k)
                <+> annotate StyleKeyword "="
                <+> prettyPrec 0 v
        PatVar NoOp x -> annotate StyleVar (pretty x)
        PatVar Op x -> annotate StyleKeyword "op" <+> annotate StyleVar (pretty x)
        PatWild -> annotate StyleKeyword "_"

instance PrettyPrec PatItem Style where
    prettyPrec _ (PatItemField p) = prettyPrec 0 p
    prettyPrec _ (PatItemVid x Nothing Nothing) = annotate StyleVar (pretty x)
    prettyPrec _ (PatItemVid x (Just t) Nothing) = annotate StyleVar (pretty x) <+> annotate StyleKeyword ":" <+> prettyPrec 0 t
    prettyPrec _ (PatItemVid x (Just t) (Just p)) = annotate StyleVar (pretty x) <+> annotate StyleKeyword ":" <+> prettyPrec 0 t <+> annotate StyleKeyword "as" <+> prettyPrec 0 p
    prettyPrec _ (PatItemVid x Nothing (Just p)) = annotate StyleVar (pretty x) <+> annotate StyleKeyword "as" <+> prettyPrec 0 p

instance PrettyPrec Ty_ Style where
    prettyPrec d = \case
        TyCon "->" [t1, t2] -> parenIf (d > 2) $
            prettyPrec 1 t1 <+> annotate StyleKeyword "->" <+> prettyPrec 2 t2
        TyCon "top" [] -> annotate StyleKeyword "top"
        TyCon "bot" [] -> annotate StyleKeyword "bot"
        TyCon c [] -> annotate StyleType (pretty c)
        TyCon c args -> tupled (prettyPrec 0 <$> args) <+> annotate StyleType (pretty c)
        TyParen t -> parens (prettyPrec 0 t)
        TyRecord r -> encloseSep (annotate StyleKeyword $ flatAlt "{ " "{") (annotate StyleKeyword $ flatAlt " }" "}") (annotate StyleKeyword ", ") $
            r <&> \(At _ (k, v)) ->
                annotate StyleLabel (pretty k)
                <+> annotate StyleKeyword ":"
                <+> prettyPrec 0 v
        TyVar x -> annotate StyleVar $ pretty x
        TyMeet t1 t2 -> prettyPrec 0 t1 <+> annotate StyleKeyword "&" <+> prettyPrec 0 t2
        TyJoin t1 t2 -> prettyPrec 0 t1 <+> annotate StyleKeyword "|" <+> prettyPrec 0 t2

instance PrettyPrec Fixity Style where
    prettyPrec _ = \case
        InfixL Nothing  -> annotate StyleKeyword "infixl"
        InfixL (Just n) -> annotate StyleKeyword "infixl" <+> annotate StyleInt (pretty n)
        InfixR Nothing  -> annotate StyleKeyword "infixr"
        InfixR (Just n) -> annotate StyleKeyword "infixr" <+> annotate StyleInt (pretty n)
        Infix Nothing   -> annotate StyleKeyword "infix"
        Infix (Just n)  -> annotate StyleKeyword "infix" <+> annotate StyleInt (pretty n)
        Nonfix          -> annotate StyleKeyword "nonfix"

instance PrettyPrec Dec_ Style where
    prettyPrec d = \case
        DecDo e -> annotate StyleKeyword "do" <+> nest 4 (prettyPrec 0 e)
        DecFixity f ids -> prettyPrec 0 f <+> hsep (map (annotate StyleVar . pretty) ids)
        DecFun (f:fs) -> vsep $ annotate StyleKeyword "fun" <+> pp f : map ((annotate StyleKeyword "and" <+>) . pp) fs
            where
                pp :: Located [FunClause] -> Doc Style
                pp (At _ (x:xs)) = vsep $ prettyPrec 0 x : map (indent 2 . (annotate StyleKeyword "|" <+>) . prettyPrec 0) xs
        DecVal (v:vs) -> vsep $ annotate StyleKeyword "val" <+> prettyPrec 0 v : map ((annotate StyleKeyword "and" <+>) . prettyPrec 0) vs
        _ -> "<error>"

instance PrettyPrec FunClause_ Style where
    prettyPrec d (FunClause pats Nothing body) = 
        hsep (map (prettyPrec 1) pats)
        <+> annotate StyleKeyword "="
        <+> nest 4 (prettyPrec 0 body)
    prettyPrec d (FunClause pats (Just t) body) = 
        hsep (map (prettyPrec 1) pats)
        <+> annotate StyleKeyword ":"
        <+> prettyPrec 0 t
        <+> annotate StyleKeyword "="
        <+> nest 4 (prettyPrec 0 body)

instance PrettyPrec ValClause_ Style where
    prettyPrec d (ValClause p e) = hsep [prettyPrec 0 p, annotate StyleKeyword "=", prettyPrec 0 e]

instance PrettyPrec Exp_ Style where
    prettyPrec d = \case
        ExpAndAlso e1 e2 -> prettyPrec 0 e1 <+> annotate StyleKeyword "andalso" <+> prettyPrec 0 e2
        ExpApp e1 e2 -> prettyPrec 0 e1 <+> prettyPrec 0 e2
        ExpConst k -> prettyPrec 0 k
        ExpConstraint e t -> prettyPrec 0 e <+> annotate StyleKeyword ":" <+> prettyPrec 0 t
        ExpFlatApp es -> hsep (map (prettyPrec 0) es)
        ExpFn (c:cs) -> vsep $ annotate StyleKeyword "fn" <+> pp c : map ((annotate StyleKeyword "|" <+>) . pp) cs
            where
                pp :: Located (Pat, Exp) -> Doc Style
                pp (At _ (p, e)) = prettyPrec 0 p <+> annotate StyleKeyword "=>" <+> prettyPrec 0 e
        ExpIf e1 e2 e3 ->
            vsep
            [ annotate StyleKeyword "if" <+> prettyPrec 0 e1
            , annotate StyleKeyword "then" <+> prettyPrec 0 e2
            , annotate StyleKeyword "else" <+> prettyPrec 0 e3
            ]
        ExpLet decs body ->
            vsep
            [ annotate StyleKeyword "let"
            , indent 4 $ vsep (prettyPrec 0 <$> decs)
            , annotate StyleKeyword "in"
            , indent 4 $ prettyPrec 0 body
            , annotate StyleKeyword "end"
            ]
        ExpOrElse e1 e2 -> prettyPrec 0 e1 <+> annotate StyleKeyword "orelse" <+> prettyPrec 0 e2
        ExpParen e -> parens (prettyPrec 0 e)
        ExpPrim p -> prettyPrec 0 p
        ExpRecord [] -> annotate StyleKeyword "()"
        ExpRecord r -> encloseSep (annotate StyleKeyword $ flatAlt "{ " "{") (annotate StyleKeyword $ flatAlt " }" "}") (annotate StyleKeyword ", ") $
            r <&> \(At _ (k, v)) ->
                annotate StyleLabel (pretty k)
                <+> annotate StyleKeyword "="
                <+> prettyPrec 0 v
        ExpGet e x -> prettyPrec 0 e <> annotate StyleKeyword "." <> annotate StyleLabel (pretty x)
        ExpSequence es -> encloseSep (annotate StyleKeyword "(") (annotate StyleKeyword ")") (annotate StyleKeyword "; ") (prettyPrec 0 <$> es)
        ExpVar Op x -> annotate StyleKeyword "op" <+> annotate StyleVar (pretty x)
        ExpVar NoOp x -> annotate StyleVar (pretty x)
        ExpFn {} -> "<error>"

instance PrettyPrec Prim_ Style where
    prettyPrec _ (Prim name ty) =
        annotate StyleKeyword "_prim"
        <+> annotate StyleString ("\"" <> pretty name <> "\"")
        <+> annotate StyleKeyword ":"
        <+> prettyPrec 0 ty
        <+> annotate StyleKeyword ";"

instance PrettyPrec Program Style where
    prettyPrec _ (Program decs) = vsep (prettyPrec 0 <$> decs)

