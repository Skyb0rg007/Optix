
{-# LANGUAGE StrictData #-}

module Language.Optix.Frontend.Syntax where

import           Data.Foldable                             (foldl')
import           Data.Function                             (on)
import           Data.List                                 (mapAccumL, sortBy)
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.Scientific                           (Scientific)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import           Data.Tuple                                (swap)
import           Numeric.Natural

import           Language.Optix.Util.Located
import           Language.Optix.Util.Pretty
import           Language.Optix.Util.Error

-- * Atoms

-- | Type Variables
type TyVar = Located TyVar_
newtype TyVar_ = TyVar { _tyVar_name :: Text }
    deriving (Show, Eq, Ord)

-- | Type Constructors
type TyCon = Located TyCon_
newtype TyCon_ = TyCon { _tyCon_name :: Text }
    deriving (Show, Eq, Ord)

-- | Constructors
type Con = Located Con_
newtype Con_ = Con { _con_name :: Text }
    deriving (Show, Eq, Ord)

-- | Vid, constructor/variable
type Vid = Located Vid_
newtype Vid_ = Vid { _vid_name :: Text }
    deriving (Show, Eq, Ord)

-- | Type Binding
type TypBind = Located TypBind_
newtype TypBind_ = TypBind [TypBindNode]
    deriving (Show, Eq, Ord)
data TypBindNode = TypBindNode
    { _typBind_tycon  :: TyCon
    , _typBind_def    :: Type
    , _typBind_tyvars :: [TyVar]
    }
    deriving (Show, Eq, Ord)

-- | Data Binding
type DatBind = Located DatBind_
data DatBind_ = DatBind
    { _datBind_datatypes :: [DatBindDatatype]
    , _datBind_withtypes :: TypBind
    }
    deriving (Show, Eq, Ord)
data DatBindDatatype = DatBindDatatype
    { _datBindDatatype_cons   :: [(Con, Maybe Type)]
    , _datBindDatatype_tycon  :: TyCon
    , _datBindDatatype_tyvars :: [TyVar]
    }
    deriving (Show, Eq, Ord)

-- | Record fields
data Field 
    = FieldSymbol Text
    | FieldInt Int
    deriving (Show, Eq, Ord)

instance Enum Field where
    toEnum = FieldInt
    fromEnum (FieldInt n) = n
    fromEnum (FieldSymbol _) = error "Field.fromEnum on FieldSymbol"

-- | Case match clauses
type Match = Located Match_
newtype Match_ = Match [(Pat, Exp)]
    deriving (Show, Eq, Ord)

-- * Constants

-- | Constants, i.e. literals
type Const = Located Const_
data Const_
    = ConstBool   Bool
    | ConstChar   Char
    | ConstInt    Integer
    | ConstReal   Scientific
    | ConstString Text
    | ConstWord   Natural
    deriving (Show, Eq, Ord)

-- | Entire Program
newtype Program = Program
    { _program_decs :: [Dec]
    }
    deriving (Show, Eq, Ord)

-- | Declarations
type Dec = Located Dec_
data Dec_
    = DecLocal [Dec] [Dec]
    | DecType TypBind
    | DecDatatype DatBind
    | DecDo Exp
    | DecFix
        { _decFix_fixity :: Fixity
        , _decFix_ops    :: [Vid]
        }
    | DecFun
        { _decFun_tyvars :: [TyVar]
        , _decFun_funBinds :: [[FunBind]]
        }
    | DecVal 
        { _decVal_tyvars   :: [TyVar]
        , _decVal_valBinds :: [(Pat, Exp)]
        }
    deriving (Show, Eq, Ord)

-- | Operator fixity
-- Difference from SML: 'infix' is not associative
-- Operators like '=' that shouldn't be combined together are able to
-- use this fixity
data Fixity
    = FixityInfix (Maybe Int)
    | FixityInfixl (Maybe Int)
    | FixityInfixr (Maybe Int)
    | FixityNonfix
    deriving (Show, Eq, Ord)

-- | Function binding
type FunBind = Located FunBind_
data FunBind_ = FunBind
    { _funBind_body :: Exp
    , _funBind_pats :: [Pat]
    , _funBind_resultType :: Maybe Type
    }
    deriving (Show, Eq, Ord)

-- | Whether an identifier is prefixed with 'op'
data Fixop = FixopOp | FixopNone
    deriving (Show, Eq, Ord)

-- | Expressions
type Exp = Located Exp_
data Exp_
    = ExpAndAlso Exp Exp
    | ExpApp Exp Exp
    | ExpCase Exp Match
    | ExpConst Const
    | ExpConstraint Exp Type
    | ExpFlatApp [Exp]
    | ExpFn Match
    | ExpIf Exp Exp Exp
    | ExpLet [Dec] Exp
    | ExpList [Exp]
    | ExpOrElse Exp Exp
    | ExpParen Exp
    | ExpRecord (Record Exp)
    | ExpSelector Field
    | ExpGet Exp Field
    | ExpSequence [Exp]
    | ExpVar Fixop Vid
    | ExpVector [Exp]
    | ExpWhile Exp Exp
    deriving (Show, Eq, Ord)

-- | Patterns
type Pat = Located Pat_
data Pat_
    = PatApp Con Pat
    | PatConst Const -- Note: cannot be REAL
    | PatConstraint Pat Type
    | PatFlatApp [Pat]
    | PatLayered Fixop Vid (Maybe Type) Pat -- [op] x [: τ] as p
    | PatList [Pat]
    | PatOr [Pat]
    | PatParen Pat
    | PatRecord [PatRecordField]
    | PatTuple [Pat]
    | PatVar Fixop Vid
    | PatVector [Pat]
    | PatWild
    deriving (Show, Eq, Ord)

-- | Record Patterns
type PatRecordField = Located PatRecordField_
data PatRecordField_
    = PatRecordFieldVid Field Vid (Maybe Type) (Maybe Pat)
    | PatRecordFieldField Field Pat
    deriving (Show, Eq, Ord)

-- | Types
type Type = Located Type_
data Type_
    = TypeCon TyCon [Type]
    | TypeParen Type
    | TypeRecord (Record Type)
    | TypeVar TyVar
    deriving (Show, Eq, Ord)

newtype Record a = Record { _record_fields :: [(Field, a)] }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- HLINT disable -}
_record_isTuple :: Record a -> Maybe [a]
_record_isTuple (Record flds) =
    let sortedFlds = sortBy (compare `on` fst) flds
     in if all
           (\(n, (f, _)) -> f == FieldInt n)
           (zip [1..] sortedFlds)
            then Just (map snd sortedFlds)
            else Nothing

data SyntaxStyle
  = StyleCon
  | StyleTyCon
  | StyleTyVar
  | StyleVid
  | StyleKeyword
  | StyleField
  | StyleBool
  | StyleChar
  | StyleInt
  | StyleReal
  | StyleString
  | StyleWord

instance StyleAnsi SyntaxStyle where
    ansiStyle = \case
        StyleCon     -> Ansi.color Ansi.Green
        StyleTyCon   -> Ansi.color Ansi.Cyan
        StyleTyVar   -> Ansi.color Ansi.Blue
        StyleVid     -> Ansi.color Ansi.Blue
        StyleKeyword -> Ansi.color Ansi.Green
        StyleField   -> Ansi.color Ansi.Blue
        StyleBool    -> Ansi.color Ansi.Red
        StyleChar    -> Ansi.color Ansi.Red
        StyleInt     -> Ansi.color Ansi.Red
        StyleReal    -> Ansi.color Ansi.Red
        StyleString  -> Ansi.color Ansi.Red
        StyleWord    -> Ansi.color Ansi.Red

prettyTyVars :: (Pretty a, Style a ~ SyntaxStyle) => [a] -> Doc SyntaxStyle
prettyTyVars [] = ""
prettyTyVars [v] = pretty v <+> ""
prettyTyVars vs = (parens . hsep $ punctuate "," (pretty <$> vs)) <+> ""

prettyFixop :: Fixop -> Doc SyntaxStyle
prettyFixop FixopOp = kw "op" <+> ""
prettyFixop FixopNone = ""

prettyMatch :: Doc SyntaxStyle -> Match_ -> Doc SyntaxStyle
prettyMatch start (Match (m:ms)) =
    vsep
    (start <+> go m : map (\m -> kw "|" <+> go m) ms)
        where
            go (p, e) = pretty p <+> kw "=>" <+> pretty e

kw :: Doc ann -> Doc SyntaxStyle
kw doc = StyleKeyword <$ doc

instance Pretty TyVar_ where
    type Style TyVar_ = SyntaxStyle
    pretty (TyVar x) = StyleTyVar <$ pretty x

instance Pretty TyCon_ where
    type Style TyCon_ = SyntaxStyle
    pretty (TyCon x) = StyleTyCon <$ pretty x

instance Pretty Con_ where
    type Style Con_ = SyntaxStyle
    pretty (Con x) = StyleCon <$ pretty x

instance Pretty Vid_ where
    type Style Vid_ = SyntaxStyle
    pretty (Vid x) = StyleVid <$ pretty x

instance Pretty TypBind_ where
    type Style TypBind_ = SyntaxStyle
    pretty (TypBind (node:nodes)) =
        vsep (kw "type" <+> pretty node : map ((kw "and" <+>) . pretty) nodes)
    pretty (TypBind []) = ""

instance Pretty TypBindNode where
    type Style TypBindNode = SyntaxStyle
    pretty TypBindNode
        { _typBind_tycon = tycon
        , _typBind_def = def
        , _typBind_tyvars = tyvars
        } =
        prettyTyVars tyvars <> pretty tycon <+> kw "=" <+> pretty def

instance Pretty DatBind_ where
    type Style DatBind_ = SyntaxStyle
    pretty DatBind
        { _datBind_datatypes = dt:dts
        , _datBind_withtypes = At _ (TypBind [])
        } =
        vsep
        (kw "datatype" <+> pretty dt : map ((kw "and" <+>) . pretty) dts)
    pretty DatBind
        { _datBind_datatypes = dt:dts
        , _datBind_withtypes = At _ (TypBind (tb:tbs))
        } =
        vsep $
        [kw "datatype" <+> pretty dt]
        ++ map ((kw "and" <+>) . pretty) dts
        ++ [kw "withtype" <+> pretty tb]
        ++ map ((kw "and" <+>) . pretty) tbs

instance Pretty DatBindDatatype where
    type Style DatBindDatatype = SyntaxStyle
    pretty DatBindDatatype
        { _datBindDatatype_cons = cons
        , _datBindDatatype_tycon = tycon
        , _datBindDatatype_tyvars = tyvars
        } =
        prettyTyVars tyvars <> pretty tycon <+> kw "=" <+> 
            hsep (punctuate (kw " |") ((\(k, v) -> pretty k <> maybe "" (\t -> kw " of" <+> pretty t) v) <$> cons))

instance Pretty Type_ where
    type Style Type_ = SyntaxStyle
    pretty (TypeCon (At _ (TyCon "->")) [a, b]) = pretty a <+> kw "->" <+> pretty b
    pretty (TypeCon con args) = prettyTyVars args <> pretty con
    pretty (TypeParen t) = parens $ pretty t
    pretty (TypeRecord (Record r))
      | Just tup <- _record_isTuple (Record r) = hsep $ punctuate (kw " *") (pretty <$> tup)
      | otherwise = braces . hsep $ punctuate (kw ",") ((\(k, v) -> pretty k <+> kw ":" <+> pretty v) <$> r)
    pretty (TypeVar x) = pretty x

instance Pretty Field where
    type Style Field = SyntaxStyle
    pretty (FieldSymbol sym) = StyleField <$ pretty sym
    pretty (FieldInt n) = StyleField <$ pretty n

instance Pretty Const_ where
    type Style Const_ = SyntaxStyle
    pretty = \case
        ConstBool b -> StyleBool <$ if b then "true" else "false"
        ConstChar c -> StyleChar <$ "#" <> dquotes (pretty c)
        ConstInt n -> StyleInt <$ pretty n
        ConstReal f -> StyleReal <$ viaShow f
        ConstString s -> StyleString <$ pretty (show s)
        ConstWord w -> StyleWord <$ "0w" <> viaShow w

instance Pretty Program where
    type Style Program = SyntaxStyle
    pretty (Program decs) = vsep (map pretty decs)

instance Pretty Dec_ where
    type Style Dec_ = SyntaxStyle
    pretty = \case
        DecLocal dec1 dec2 -> vsep
            [ kw "local"
            , indent 2 $ vsep $ map pretty dec1
            , kw "in"
            , indent 2 $ vsep $ map pretty dec2
            , kw "end"
            ]
        DecType tb -> pretty tb
        DecDatatype db -> pretty db
        DecDo exp -> kw "do" <+> pretty exp
        DecFix { _decFix_fixity = fixity, _decFix_ops = ops } ->
            pretty fixity <+> hsep (map pretty ops)
        DecFun { _decFun_tyvars = tyvars, _decFun_funBinds = fbs } ->
            prettyTyVars tyvars <> prettyFunBindss fbs
        DecVal { _decVal_tyvars = tyvars, _decVal_valBinds = vb:vbs } ->
            prettyTyVars tyvars <> nest 2 (vsep (kw "val" <+> prettyValBind vb : map ((kw "and" <+>) . prettyValBind) vbs))

        where
            prettyValBind :: (Pat, Exp) -> Doc SyntaxStyle
            prettyValBind (p, e) = pretty p <+> kw "=" <+> pretty e
            prettyFunBindss :: [[FunBind]] -> Doc SyntaxStyle
            prettyFunBindss (fb:fbs) =
                vsep (prettyFunBinds (kw "fun") fb : map (prettyFunBinds (kw "and")) fbs)
            prettyFunBinds :: Doc SyntaxStyle -> [FunBind] -> Doc SyntaxStyle
            prettyFunBinds start (fb:fbs) =
                vsep (start <+> prettyFunBind fb : map ((kw "  |" <+>) . prettyFunBind) fbs)
            prettyFunBind :: FunBind -> Doc SyntaxStyle
            prettyFunBind (At _ FunBind { _funBind_body = body, _funBind_pats = pats, _funBind_resultType = resultType }) =
                hsep (map pretty pats)
                <+> (case resultType of { Nothing -> ""; Just t -> pretty t <+> "" })
                <> kw "=" <+> nest 2 (pretty body)

instance Pretty Pat_ where
    type Style Pat_ = SyntaxStyle
    pretty = \case
        PatApp con pat -> pretty con <+> pretty pat
        PatConst k -> pretty k
        PatConstraint p t -> pretty p <+> kw ":" <+> pretty t
        PatFlatApp pats -> hsep (map pretty pats)
        PatLayered fixop vid mType pat ->
            prettyFixop fixop
            <> pretty vid
            <+> maybe "" (\t -> kw ":" <+> pretty t) mType
            <+> kw "as"
            <+> pretty pat
        PatList pats -> list (map pretty pats)
        PatOr pats -> hsep $ punctuate (kw " |") (map pretty pats)
        PatParen p -> kw "(" <> pretty p <> kw ")"
        PatRecord prf -> kw "{" <+> hsep (punctuate (kw ",") (map pretty prf)) <+> kw "}"
        PatTuple pats -> kw "(" <> hsep (punctuate (kw ",") (map pretty pats)) <> kw ")"
        PatVar fixop x -> prettyFixop fixop <> pretty x
        PatVector pats -> kw "#[" <> hsep (punctuate (kw ",") (map pretty pats)) <> kw "]"
        PatWild -> kw "_"

instance Pretty PatRecordField_ where
    type Style PatRecordField_ = SyntaxStyle
    pretty = \case
        PatRecordFieldField f p -> pretty f <+> kw "=" <+> pretty p
        PatRecordFieldVid f _ Nothing Nothing   -> pretty f
        PatRecordFieldVid f _ (Just t) Nothing  -> pretty f <+> kw ":" <+> pretty t
        PatRecordFieldVid f _ (Just t) (Just p) -> pretty f <+> kw ":" <+> pretty t <+> kw "as" <+> pretty p
        PatRecordFieldVid f _ Nothing (Just p)  -> pretty f <+> kw "as" <+> pretty p

instance Pretty Fixity where
    type Style Fixity = SyntaxStyle
    pretty = \case
        FixityInfix Nothing   -> kw "infix"
        FixityInfix (Just n)  -> kw "infix" <+> (StyleInt <$ pretty n)
        FixityInfixl Nothing  -> kw "infixl"
        FixityInfixl (Just n) -> kw "infixl" <+> (StyleInt <$ pretty n)
        FixityInfixr Nothing  -> kw "infixr"
        FixityInfixr (Just n) -> kw "infixr" <+> (StyleInt <$ pretty n)
        FixityNonfix          -> kw "nonfix"

instance Pretty Exp_ where
    type Style Exp_ = SyntaxStyle
    pretty = \case
        ExpAndAlso e1 e2 -> pretty e1 <+> kw "andalso" <+> pretty e2
        ExpApp e1 e2 -> pretty e1 <+> pretty e2
        ExpCase e (At _ m) -> kw "case" <+> pretty e <+> kw "of" <> hardline <> indent 2 (prettyMatch " " m)
        ExpConst k -> pretty k
        ExpConstraint e t -> pretty e <+> kw ":" <+> pretty t
        ExpFlatApp es -> hsep (map pretty es)
        ExpFn (At _ m) -> prettyMatch "fn" m
        ExpIf e1 e2 e3 -> kw "if" <+> pretty e1 <+> kw "then" <+> pretty e2 <+> kw "else" <+> pretty e3
        ExpLet ds e ->
            vsep
            [ kw "let"
            , indent 2 $ vsep $ map pretty ds
            , kw "in"
            , indent 2 (pretty e)
            , kw "end"
            ]
        ExpList es -> list (map pretty es)
        ExpOrElse e1 e2 -> pretty e1 <+> kw "orelse" <+> pretty e2
        ExpParen e -> kw "(" <> pretty e <> kw ")"
        ExpRecord (Record r)
          | Just tup <- _record_isTuple (Record r) -> kw "(" <> hsep (punctuate "," (map pretty tup)) <> kw ")"
          | otherwise -> kw "{" <+> hsep (punctuate "," (map (\(k, v) -> pretty k <+> kw "=" <+> pretty v) r)) <+> kw "}"
        ExpSelector fld -> StyleField <$ "#" <> pretty fld
        ExpGet e fld -> pretty e <> kw "." <> (StyleField <$ pretty fld)
        ExpSequence es -> kw "(" <+> hsep (punctuate ";" (map pretty es)) <+> kw ")"
        ExpVar fixop x -> prettyFixop fixop <> (StyleVid <$ pretty x)
        ExpVector es -> kw "#[" <+> hsep (punctuate "," (map pretty es)) <+> kw "]"
        ExpWhile e1 e2 -> kw "while" <+> pretty e1 <+> kw "do" <+> pretty e2


