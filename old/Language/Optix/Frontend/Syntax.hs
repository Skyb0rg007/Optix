{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Optix.Frontend.Syntax where

import           Control.DeepSeq           (NFData)
import           Control.Exception         (assert)
import           Data.Binary               (Binary (..))
import           Data.Data                 (Data)
import           Data.Functor              ((<&>))
import           Data.Int                  (Int32)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc as Pretty
import           Data.Typeable             (Typeable)
import qualified Foreign.LibFFI            as LibFFI
import           GHC.Generics              (Generic)
import           GHC.Stack                 (HasCallStack)
import           Text.Megaparsec.Pos       (SourcePos (..), unPos)

import           Language.Optix.Utils.Located
import           Language.Optix.Utils.Pretty

-- * Program AST

newtype Program = Program
    { _programDecls :: [Decl]
    }
    deriving (Eq, Ord, Generic, Data, Typeable, NFData, Binary)
instance Show Program where
    showsPrec _ (Program decls) = showList decls

type Decl = Located Decl_
data Decl_
    = DeclFun !(NonEmpty FunDecl)
    | DeclVal !(NonEmpty ValDecl)
    | DeclFixity !FixityDecl
    | DeclDo !Exp
    -- | DeclLocal ![Decl] ![Decl]
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

type Pat = Located Pat_
data Pat_
    = PatWild
    | PatVar !Text !Bool !(Maybe Ty) -- op + : t
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

data ValDecl = ValDecl
    { _valDeclPat  :: !Pat
    , _valDeclBody :: !Exp
    }
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

data FunDecl = FunDecl
    { _funDeclPats    :: !(NonEmpty Pat)
    , _funDeclRetType :: !(Maybe Ty)
    , _funDeclBody    :: !Exp
    }
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

data Fixity
    = Infix  !Int
    | Infixl !Int
    | Infixr !Int
    | Nonfix
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

data FixityDecl = FixityDecl
    { _fixityDeclType :: !Fixity
    , _fixityDeclIds  :: !(NonEmpty Text)
    }
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

type Exp = Located Exp_
data Exp_
    = ExpFlatApp !(NonEmpty Exp)
    | ExpApp !Exp !Exp
    | ExpVar {- name -} !Text {- preceded by 'op' -} !Bool
    | ExpInt !Int32
    | ExpBool !Bool
    | ExpUnit
    | ExpIf !Exp !Exp !Exp
    | ExpFn !Pat !Exp
    | ExpLet ![Decl] !Exp
    | ExpRecord ![(Text, Exp)]
    | ExpGet !Exp !Text
    | ExpConstraint !Exp !Ty
    | ExpPrim !Text !Ty
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

type Ty = Located Ty_
data Ty_
    = TyVar !Text
    | TyArrow !Ty !Ty
    | TyRecord ![(Text, Ty)]
    | TyInt
    | TyBool
    | TyUnit
    | TyTop
    | TyBot
    | TyMeet !Ty !Ty
    | TyJoin !Ty !Ty
    | TyRecursive !Text !Ty
    deriving (Show, Eq, Ord, Generic, Data, Typeable, NFData, Binary)

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

instance PrettyPrec Ty_ Style where
    prettyPrec d = \case
        TyVar x -> annotate StyleTyVar $ pretty x
        TyArrow a b -> parenIf (d > 1) $
            prettyPrec 2 a <+> annotate StyleKeyword "->" <+> prettyPrec 1 b
        TyRecord [] -> "{}"
        TyRecord r ->
            encloseSep
            (annotate StyleKeyword "{")
            (annotate StyleKeyword "}")
            (annotate StyleKeyword ", ")
            (r <&> \(k, v) -> annotate StyleLabel (pretty k) <+> annotate StyleKeyword ":" <+> prettyPrec 0 v)
        TyInt -> annotate StyleType "int"
        TyBool -> annotate StyleType "bool"
        TyUnit -> annotate StyleType "()"
        TyBot -> annotate StyleType "bot"
        TyTop -> annotate StyleType "top"
        TyMeet a b -> parenIf (d > 2) $
            prettyPrec 2 a <+> annotate StyleKeyword "&" <+> prettyPrec 2 b
        TyJoin a b -> parenIf (d > 2) $
            prettyPrec 2 a <+> annotate StyleKeyword "|" <+> prettyPrec 2 b
        TyRecursive u t -> parenIf (d > 0) $
            annotate StyleKeyword "@"
            <> annotate StyleTyVar (pretty u)
            <> annotate StyleKeyword "."
            <> prettyPrec 0 t

instance PrettyPrec Exp_ Style where
    prettyPrec d = \case
        ExpFlatApp (e:|[]) -> prettyPrec d e
        ExpFlatApp (e:|es) -> parenIf (d > 1) $
            prettyPrec 1 e <+> hsep (map (prettyPrec 2) es)
        ExpApp e1 e2 -> parenIf (d > 1) $
            prettyPrec 1 e1 <+> prettyPrec 2 e2
        ExpVar x False -> annotate StyleVar $ pretty x
        ExpVar x True -> annotate StyleKeyword "op" <+> annotate StyleVar (pretty x)
        ExpInt n -> annotate StyleInt $ pretty n
        ExpBool True -> annotate StyleBool "true"
        ExpBool False -> annotate StyleBool "false"
        ExpUnit -> annotate StyleKeyword "()"
        ExpIf e1 e2 e3 -> parenIf (d > 0) $
            annotate StyleKeyword "if"
            <+> prettyPrec 0 e1
            <+> annotate StyleKeyword "then"
            <+> prettyPrec 0 e2
            <+> annotate StyleKeyword "else"
            <+> prettyPrec 0 e3
        ExpFn p e -> parenIf (d > 0) $
            annotate StyleKeyword "fn"
            <+> annotate StyleVar (prettyPrec 0 p)
            <+> annotate StyleKeyword "=>"
            <+> prettyPrec 0 e
        ExpLet decls e -> parenIf (d > 0) $
            vsep
            [ annotate StyleKeyword "let"
            , indent 4 (vsep (prettyPrec 0 <$> decls))
            , annotate StyleKeyword "in"
            , indent 4 (prettyPrec 0 e)
            , annotate StyleKeyword "end"
            ]
        ExpRecord [] -> annotate StyleKeyword "{}"
        ExpRecord r ->
            encloseSep
            (annotate StyleKeyword "{")
            (annotate StyleKeyword "}")
            (annotate StyleKeyword ", ")
            (r <&> \(k, v) -> annotate StyleLabel (pretty k) <+> annotate StyleKeyword "=" <+> prettyPrec 0 v)
        ExpGet e x ->
            prettyPrec 2 e <> annotate StyleKeyword "." <> annotate StyleLabel (pretty x)
        ExpConstraint e t -> parenIf (d > 0) $
            prettyPrec 0 e <+> annotate StyleKeyword ":" <+> prettyPrec 0 t
        ExpPrim x t -> parenIf (d > 0) $
            "prim: " <> pretty x <+> ":" <+> prettyPrec 0 t

instance PrettyPrec Decl_ Style where
    prettyPrec d = \case
        DeclVal (v:|vs) ->
            let go x (ValDecl pat exp) =
                    annotate StyleKeyword x
                    <+> prettyPrec 0 pat
                    <+> annotate StyleKeyword "="
                    <+> align (prettyPrec 0 exp)
             in vsep (go "val" v : map (go "and") vs)
        DeclFun (f:|fs) ->
            let go x (FunDecl pats mRetTy body) =
                    let ty = case mRetTy of
                               Nothing -> ""
                               Just ty -> annotate StyleKeyword ":" <+> prettyPrec 0 ty <+> ""
                     in annotate StyleKeyword x
                        <+> hsep (prettyPrec 1 <$> NonEmpty.toList pats)
                        <+> ty
                        <> annotate StyleKeyword "="
                        <+> align (prettyPrec 0 body)
             in vsep (go "fun" f : map (go "and") fs)
        DeclFixity (FixityDecl fixity ids) ->
            let fixity' = case fixity of
                            Infix n -> annotate StyleKeyword "infix" <+> pretty n
                            Infixl n -> annotate StyleKeyword "infixl" <+> pretty n
                            Infixr n -> annotate StyleKeyword "infixr" <+> pretty n
             in fixity' <+> hsep (map pretty (NonEmpty.toList ids))
        DeclDo e ->
            annotate StyleKeyword "do" <+> prettyPrec 0 e

instance PrettyPrec Program Style where
    prettyPrec _ (Program decs) = vsep (prettyPrec 0 <$> decs)

instance PrettyPrec Pat_ Style where
    prettyPrec d = \case
        PatWild -> annotate StyleKeyword "_"
        PatVar "" _ _ -> error "Empty pattern variable"
        PatVar x False Nothing -> annotate StyleVar (pretty x)
        PatVar x False (Just ty) -> parenIf (d > 0) $
            annotate StyleVar (pretty x) <+> annotate StyleKeyword ":" <+> prettyPrec 0 ty
        PatVar x True Nothing ->
            annotate StyleKeyword "op" <+> annotate StyleVar (pretty x)
        PatVar x op (Just t) -> parenIf (d > 0) $
            annotate StyleKeyword "op"
            <+> annotate StyleVar (pretty x)
            <+> annotate StyleKeyword ":"
            <+> prettyPrec 0 t
            <> if d > 0 && Text.last x == '*' then " " else ""


