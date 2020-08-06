{
-- vim: set ft=haskell ts=2 sw=2:

-- TODO: Implement %changes


-- Implemented extensions:
--
-- Do declarations
-- Optional trailing semicolon in 'let' expressions
-- Optional beginning '|' in matches

module Language.Optix.Frontend.Parser.Parser
    ( parseProgram
    , parseExp
    ) where

import qualified Data.ByteString.Lazy       as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy.ByteString.Char8
import           Data.Foldable              (foldl')
import           Data.List                  (sort)
import qualified Data.Map                   as Map
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Numeric.Natural            (Natural)

import           Language.Optix.Frontend.Parser.Lexer (lexer)
import           Language.Optix.Frontend.Parser.Monad (AlexInput, Parser)
import           Language.Optix.Frontend.Parser.Token (Token, NumRadix)
import qualified Language.Optix.Frontend.Parser.Token as Token
import           Language.Optix.Frontend.Syntax
import           Language.Optix.Util.Pretty
import           Language.Optix.Util.Error
import           Language.Optix.Util.Located
}

%monad { Parser } { (>>=) } { pure }
%lexer { lexer } { At _ Token.EOF }
%tokentype { Token }
%error { parseError }

-- TODO: fix shift/reduce conflicts
%expect 2

%token
  HASH         { At $$ Token.HASH         }
  HASHLBRACKET { At $$ Token.HASHLBRACKET }
  LPAREN       { At $$ Token.LPAREN       }
  RPAREN       { At $$ Token.RPAREN       }
  COMMA        { At $$ Token.COMMA        }
  ARROW        { At $$ Token.ARROW        }
  DOT          { At $$ Token.DOT          }
  COLON        { At $$ Token.COLON        }
  SEMICOLON    { At $$ Token.SEMICOLON    }
  EQUALOP      { At $$ Token.EQUALOP      }
  DARROW       { At $$ Token.DARROW       }
  LBRACKET     { At $$ Token.LBRACKET     }
  RBRACKET     { At $$ Token.RBRACKET     }
  WILD         { At $$ Token.WILD         }
  LBRACE       { At $$ Token.LBRACE       }
  BAR          { At $$ Token.BAR          }
  RBRACE       { At $$ Token.RBRACE       }
  AMPERSAND    { At $$ Token.AMPERSAND    }
  ASTERISK     { At $$ Token.ASTERISK     }

  AND      { At $$ Token.AND      }
  ANDALSO  { At $$ Token.ANDALSO  }
  AS       { At $$ Token.AS       }
  CASE     { At $$ Token.CASE     }
  DATATYPE { At $$ Token.DATATYPE }
  DO       { At $$ Token.DO       }
  ELSE     { At $$ Token.ELSE     }
  END      { At $$ Token.END      }
  FN       { At $$ Token.FN       }
  FUN      { At $$ Token.FUN      }
  IF       { At $$ Token.IF       }
  IN       { At $$ Token.IN       }
  INFIX    { At $$ Token.INFIX    }
  INFIXL   { At $$ Token.INFIXL   }
  INFIXR   { At $$ Token.INFIXR   }
  LET      { At $$ Token.LET      }
  LOCAL    { At $$ Token.LOCAL    }
  NONFIX   { At $$ Token.NONFIX   }
  OF       { At $$ Token.OF       }
  OP       { At $$ Token.OP       }
  ORELSE   { At $$ Token.ORELSE   }
  THEN     { At $$ Token.THEN     }
  TYPE     { At $$ Token.TYPE     }
  VAL      { At $$ Token.VAL      }
  WHILE    { At $$ Token.WHILE    }
  WITHTYPE { At $$ Token.WITHTYPE }

  ID      { AtTokenID      $$ }
  TYVARID { AtTokenTYVARID $$ }
  SYMID   { AtTokenSYMID   $$ }

  REAL   { AtTokenREAL   $$ }
  INT    { AtTokenINT    $$ }
  WORD   { AtTokenWORD   $$ }
  STRING { AtTokenSTRING $$ }
  CHAR   { AtTokenCHAR   $$ }

%name parseProgram program
%name parseExp     exp

%nonassoc WITHTYPE
%right AND
%right ARROW
%right DARROW
%left DO
%left ELSE
%left ORELSE
%left ANDALSO
%right AS
%left COLON

%%

program :: { Program }
  : topdecs { Program $1 }

-- expsAndTopdecs :: { [Dec] }
  -- : exp SEMICOLON expsAndTopdecs { At (spanOf $1) (DecExp $1) : $3 }
  -- | topdecs                      { $1 }

topdecs :: { [Dec] }
  : dec decs { $1 : $2 }
  | SEMICOLON topdecs { $2 }

dec :: { Dec }
  : decnolocal             { $1 }
  | LOCAL decs IN decs END { At ($1 <> $5) $ DecLocal $2 $4 }

decnolocal :: { Dec }
  : VAL valbind          { At ($1 <> spanOf $2) $ DecVal { _decVal_tyvars = [], _decVal_valBinds = valOf $2 } }
  | VAL tyvarseq valbind { At ($1 <> spanOf $3) $ DecVal { _decVal_tyvars = valOf $2, _decVal_valBinds = valOf $3 } }
  | DO exp               { At ($1 <> spanOf $2) $ DecDo $2 }
  | FUN funs             { At ($1 <> spanOf (last $ last $2)) $ DecFun { _decFun_tyvars = [], _decFun_funBinds = $2 } }
  | FUN tyvarseq funs    { At ($1 <> spanOf (last $ last $3)) $ DecFun { _decFun_tyvars = valOf $2, _decFun_funBinds = $3 } }
  | TYPE typBind         { At ($1 <> spanOf $2) $ DecType $2 }
  | DATATYPE datBind     { At ($1 <> spanOf $2) $ DecDatatype $2 }
  | fixity vids          { At (spanOf $1 <> spanOf (last $2)) $ DecFix { _decFix_fixity = valOf $1, _decFix_ops = $2 } }

--
-- * Declarations
--

decs :: { [Dec] }
  : {- empty -}    { [] }
  | SEMICOLON decs { $2 }
  | dec decs       { $1 : $2 }

valbind :: { Located [(Pat, Exp)] }
  : pat EQUALOP exp             { At (spanOf $1 <> spanOf $3) [($1, $3)] }
  | pat EQUALOP exp AND valbind { At (spanOf $1 <> spanOf $5) (($1, $3) : valOf $5) }

constraint :: { Maybe Type }
  : {- empty -} { Nothing }
  | COLON ty    { Just $2 }

funs :: { [[FunBind]] }
  : clausesTop          { [$1] }
  | clausesTop AND funs { $1 : $3 }

clausesTop :: { [FunBind] }
  : optbar clauses { $2 }

clauses :: { [FunBind] }
  : clause             { [$1] }
  | clause BAR clauses { $1 : $3 }

clause :: { FunBind }
  : apats constraint EQUALOP exp {
      At (spanOf (head $1) <> spanOf $4) $
        FunBind { _funBind_pats = $1, _funBind_resultType = $2, _funBind_body = $4 }
  }

typBind :: { TypBind }
  : tbs { At (spanOf $1) $ TypBind (valOf $1) }

tbs :: { Located [TypBindNode] }
  : tb         { At (spanOf $1) [valOf $1] }
  | tb AND tbs { At (spanOf $1 <> spanOf $3) (valOf $1 : valOf $3) }

tb :: { Located TypBindNode }
  : tyvarseq tycon EQUALOP ty {
      At (spanOf $1 <> spanOf $4) TypBindNode { _typBind_tyvars = valOf $1, _typBind_tycon = $2, _typBind_def = $4 }
  }
  | tycon EQUALOP ty {
      At (spanOf $1 <> spanOf $3) TypBindNode { _typBind_tyvars = [], _typBind_tycon = $1, _typBind_def = $3 }
  }

tyvarseq :: { Located [TyVar] }
  : tyvar                  { At (spanOf $1) [$1] }
  | LPAREN tyvar_pc RPAREN { At ($1 <> $3) $2 }

tyvar_pc :: { [TyVar] }
  : tyvar                { [$1] }
  | tyvar COMMA tyvar_pc { $1 : $3 }

constrs :: { Located [(Con, Maybe Type)] }
  : constr             { At (spanOf $1) [valOf $1] }
  | constr BAR constrs { At (spanOf $1 <> spanOf $3) (valOf $1 : valOf $3) }

constr :: { Located (Con, Maybe Type) }
  : opcon       { At (spanOf $1) ($1, Nothing) }
  | opcon OF ty { At (spanOf $1 <> spanOf $3) ($1, Just $3) }

opcon :: { Con }
  : con    { $1 }
  | OP con { At ($1 <> spanOf $2) (valOf $2) }

fixity :: { Located Fixity }
  : INFIX        { At $1                $ FixityInfix Nothing }
  | INFIX digit  { At ($1 <> spanOf $2) $ FixityInfix (Just (valOf $2)) }
  | INFIXL       { At $1                $ FixityInfixl Nothing }
  | INFIXL digit { At ($1 <> spanOf $2) $ FixityInfixl (Just (valOf $2)) }
  | INFIXR       { At $1                $ FixityInfixr Nothing }
  | INFIXR digit { At ($1 <> spanOf $2) $ FixityInfixr (Just (valOf $2)) }
  | NONFIX       { At $1                $ FixityNonfix }

digit :: { Located Int }
  : INT {%
      let At s (n, radix, lbs) = $1
       in if radix == Token.DEC && Lazy.ByteString.Char8.length lbs == 1
             then pure $ At s (fromInteger n)
             else do
               withCurrentSpan s $
                 compilerExternalWarning "Invalid digit in infix declaration"
               pure $ At s 0
  }

datBind :: { DatBind }
  : dbs           { At (spanOf $1)              $ DatBind { _datBind_datatypes = valOf $1, _datBind_withtypes = At BogusSpan (TypBind []) } }
  | dbs withtypes { At (spanOf $1 <> spanOf $2) $ DatBind { _datBind_datatypes = valOf $1, _datBind_withtypes = $2 } }

dbs :: { Located [DatBindDatatype] }
  : db         { At (spanOf $1) [valOf $1] }
  | db AND dbs { At (spanOf $1 <> spanOf $3) $ valOf $1 : valOf $3 }

db :: { Located DatBindDatatype }
  : tycon EQUALOP constrs              { At (spanOf $1 <> spanOf $3) $ DatBindDatatype { _datBindDatatype_tyvars = [],       _datBindDatatype_tycon = $1, _datBindDatatype_cons = valOf $3 } }
  | tyvarseq tycon EQUALOP constrs     { At (spanOf $1 <> spanOf $4) $ DatBindDatatype { _datBindDatatype_tyvars = valOf $1, _datBindDatatype_tycon = $2, _datBindDatatype_cons = valOf $4 } }
  | tycon EQUALOP BAR constrs          { At (spanOf $1 <> spanOf $4) $ DatBindDatatype { _datBindDatatype_tyvars = [],       _datBindDatatype_tycon = $1, _datBindDatatype_cons = valOf $4 } }
  | tyvarseq tycon EQUALOP BAR constrs { At (spanOf $1 <> spanOf $5) $ DatBindDatatype { _datBindDatatype_tyvars = valOf $1, _datBindDatatype_tycon = $2, _datBindDatatype_cons = valOf $5 } }

withtypes :: { TypBind }
  : WITHTYPE typBind { At ($1 <> spanOf $2) $ valOf $2 }

--
-- * Patterns
--

pat :: { Pat }
  : cpat BAR barcpats { At (spanOf $1 <> spanOf (last $3)) $ PatOr ($1 : $3) }
  | cpat              { $1 }

cpat :: { Pat }
  : cpat AS cpat  {% At (spanOf $1 <> spanOf $3) `fmap` makePatAs $1 $3 }
  | cpat COLON ty { At (spanOf $1 <> spanOf $3) $ PatConstraint $1 $3 }
  | apats         { At (spanOf (head $1) <> spanOf (last $1)) $ PatFlatApp $1 }

apats :: { [Pat] }
  : apat       { [$1] }
  | apat apats { $1 : $2 }

apat :: { Pat }
  : vidNoEqual { At (spanOf $1) $ PatVar FixopNone $1 }
  | OP vid     { At ($1 <> spanOf $2) $ PatVar FixopOp $2 }
  | const      {% do
      case $1 of
        At s (ConstReal _) ->
          withCurrentSpan s $
            compilerExternalWarning "real constants not allowed in patterns"
        _ -> pure ()
      pure $ At (spanOf $1) (PatConst $1)
  }
  | WILD                       { At $1 PatWild }
  | LPAREN pats RPAREN         { At ($1 <> $3) $ PatTuple $2 }
  | LBRACKET pats RBRACKET     { At ($1 <> $3) $ PatList $2 }
  | HASHLBRACKET pats RBRACKET { At ($1 <> $3) $ PatVector $2 }
  | LBRACE RBRACE              { At ($1 <> $2) $ PatTuple [] }
  | LBRACE patitems RBRACE     { At ($1 <> $3) $ PatRecord $2 }

pats :: { [Pat] }
  : {- empty -}   { [] }
  | pat commapats { $1 : $2 }

patitems :: { [PatRecordField] }
  : patitem COMMA patitems { $1 : $3 }
  | patitem                { [$1] }

patitem :: { PatRecordField }
  : field EQUALOP pat     { At (spanOf $1 <> spanOf $3) $ PatRecordFieldField (valOf $1) $3 }
  | field                 { At (spanOf $1)              $ PatRecordFieldVid (valOf $1) (fieldToVid $1) Nothing Nothing }
  | field COLON ty        { At (spanOf $1 <> spanOf $3) $ PatRecordFieldVid (valOf $1) (fieldToVid $1) (Just $3) Nothing }
  | field AS pat          { At (spanOf $1 <> spanOf $3) $ PatRecordFieldVid (valOf $1) (fieldToVid $1) Nothing (Just $3) }
  | field COLON ty AS pat { At (spanOf $1 <> spanOf $5) $ PatRecordFieldVid (valOf $1) (fieldToVid $1) (Just $3) (Just $5) }

commapats :: { [Pat] }
  : {- empty -}         { [] }
  | COMMA pat commapats { $2 : $3 }

barcpats :: { [Pat] }
  : cpat              { [$1] }
  | cpat BAR barcpats { $1 : $3 }

exp :: { Exp }
  : exp ORELSE exp           { At (spanOf $1 <> spanOf $3) $ ExpOrElse $1 $3 }
  | exp ANDALSO exp          { At (spanOf $1 <> spanOf $3) $ ExpAndAlso $1 $3 }
  | exp COLON ty             { At (spanOf $1 <> spanOf $3) $ ExpConstraint $1 $3 }
  | app_exp                  { At (spanOf (head $1) <> spanOf (last $1)) $ ExpFlatApp $1 }
  | FN match                 { At ($1 <> spanOf $2) $ ExpFn $2 }
  | CASE exp OF match        { At ($1 <> spanOf $4) $ ExpCase $2 $4 }
  | WHILE exp DO exp         { At ($1 <> spanOf $4) $ ExpWhile $2 $4 }
  | IF exp THEN exp ELSE exp { At ($1 <> spanOf $6) $ ExpIf $2 $4 $6 }

app_exp :: { [Exp] }
  : get_exp         { [$1] }
  | get_exp app_exp { $1 : $2 }
  -- | vid          { [At (spanOf $1) (ExpVar FixopNone $1)] }
  -- | vid app_exp  { At (spanOf $1) (ExpVar FixopNone $1) : $2 }

get_exp :: { Exp }
  : aexp get { foldl' (\e@(At s2 _) (At s1 fld) -> At (s1 <> s2) (ExpGet e fld)) $1 $2 }

get :: { [Located Field] }
  : {- empty -}   { [] }
  | DOT field get { $2 : $3 }

aexp :: { Exp }
  : OP vid                         { At ($1 <> spanOf $2) $ ExpVar FixopOp $2 }
  | vid                            { At (spanOf $1) $ ExpVar FixopNone $1 }
  | const                          { At (spanOf $1) $ ExpConst $1 }
  | HASH field                     { At ($1 <> spanOf $2) $ ExpSelector (valOf $2) }
  | HASHLBRACKET exp_list RBRACKET { At ($1 <> $3) $ ExpVector $2 }
  | HASHLBRACKET RBRACKET          { At ($1 <> $2) $ ExpVector [] }
  | LBRACE elabels RBRACE          { At ($1 <> $3) $ ExpRecord (Record $2) }
  | LBRACE RBRACE                  { At ($1 <> $2) $ ExpRecord (Record []) }
  | LPAREN RPAREN                  { At ($1 <> $2) $ ExpRecord (Record []) }
  | LPAREN exp_ps RPAREN           {
      At ($1 <> $3) $ case $2 of
                        [exp] -> ExpParen exp
                        _ -> ExpSequence $2
  }
  | LPAREN exp_2c RPAREN           { At ($1 <> $3) $ ExpRecord (Record (zip [FieldInt 1..] $2)) }
  | LBRACKET exp_list RBRACKET     { At ($1 <> $3) $ ExpList $2 }
  | LBRACKET RBRACKET              { At ($1 <> $2) $ ExpList [] }
  | LET decs IN exp_ps END         {
      At ($1 <> $5) $ ExpLet $2 $ case $4 of
                                       [exp] -> exp
                                       _ -> At (spanOf (head $4) <> spanOf (last $4)) $ ExpSequence $4
  }

exp_2c :: { [Exp] }
  : exp COMMA exp_2c { $1 : $3 }
  | exp COMMA exp    { [$1, $3] }

exp_ps :: { [Exp] }
  : exp optsemicolon     { [$1] }
  | exp SEMICOLON exp_ps { $1 : $3 }

elabel :: { (Field, Exp) }
  : field EQUALOP exp { (valOf $1, $3) }

elabels :: { [(Field, Exp)] }
  : elabel COMMA elabels { $1 : $3 }
  | elabel               { [$1] }

exp_list :: { [Exp] }
  : exp                { [$1] }
  | exp COMMA exp_list { $1 : $3 }

match :: { Match }
  : rules     { At (spanOf (fst $ head $1) <> spanOf (snd $ last $1)) $ Match $1 }
  | BAR rules { At ($1 <> spanOf (snd $ last $2)) $ Match $2 }

rules :: { [(Pat, Exp)] }
  : rule           { [$1] }
  | rule BAR rules { $1 : $3 }

rule :: { (Pat, Exp) }
  : pat DARROW exp { ($1, $3) }

--
-- * Types
--

ty :: { Type }
  : tynode { $1 }

tynode :: { Type }
  : tuple_ty    { At (spanOf (head $1) <> spanOf (last $1)) $ TypeRecord (Record (zip [FieldInt 1..] $1)) }
  | ty ARROW ty { At (spanOf $1 <> spanOf $3) $ TypeCon (At $2 (TyCon "->")) [$1, $3] }
  | ty1         { $1 }

ty1 :: { Type }
  : tyvar                      { At (spanOf $1) $ TypeVar $1 }
  | LBRACE RBRACE              { At ($1 <> $2) $ TypeRecord (Record []) }
  | LBRACE tlabels RBRACE      { At ($1 <> $3) $ TypeRecord (Record $2) }
  | LPAREN ty0_pc RPAREN tycon { At ($1 <> spanOf $4) $ TypeCon $4 $2 }
  | LPAREN ty RPAREN           { At ($1 <> $3) $ TypeParen $2 }
  | ty1 tycon                  { At (spanOf $1 <> spanOf $2) $ TypeCon $2 [$1] }
  | tycon                      { At (spanOf $1) $ TypeCon $1 [] }

tlabel :: { (Field, Type) }
  : field COLON ty { (valOf $1, $3) }

tlabels :: { [(Field, Type)] }
  : tlabel COMMA tlabels { $1 : $3 }
  | tlabel               { [$1] }

tuple_ty :: { [Type] }
  : ty1 ASTERISK tuple_ty { $1 : $3 }
  | ty1 ASTERISK ty1      { [$1, $3] }

ty0_pc :: { [Type] }
  : ty COMMA ty     { [$1, $3] }
  | ty COMMA ty0_pc { $1 : $3 }

--
-- * Atoms
--

optbar :: { () }
  : {- empty -} { () }
  | BAR         { () }

optsemicolon :: { () }
  : {- empty -} { () }
  | SEMICOLON   { () }

con :: { Con }
  : vid { let At s (Vid x) = $1 in At s (Con x) }

tyvar :: { TyVar }
  : TYVARID { At (spanOf $1) $ TyVar (valOf $1) }

vidNoEqual :: { Vid }
  : ID        { At (spanOf $1) $ Vid (valOf $1) }
  | SYMID     { At (spanOf $1) $ Vid (valOf $1) }
  | ASTERISK  { At $1 $ Vid "*" }
  | AMPERSAND { At $1 $ Vid "&" }

vid :: { Vid }
  : vidNoEqual { $1 }
  | EQUALOP    { At $1 $ Vid "=" }

vids :: { [Vid] }
  : vid      { [$1] }
  | vid vids { $1 : $2 }

tycon :: { TyCon }
  : ID    { At (spanOf $1) $ TyCon (valOf $1) }
  | SYMID { At (spanOf $1) $ TyCon (valOf $1) }

idField :: { Located Text }
  : ID        { $1 }
  | SYMID     { $1 }
  | ASTERISK  { At $1 "*" }
  | AMPERSAND { At $1 "&" }

numericField :: { Located Int }
  : INT {%
    let At s (n, radix, lbs) = $1
     in if and
           [ radix == Token.DEC
           , Lazy.ByteString.Char8.length lbs > 0
           , Lazy.ByteString.Char8.head lbs /= '0'
           , n < toInteger (maxBound :: Int)
           ]
           then pure (At s (fromInteger n))
           else do
             withCurrentSpan s $
               compilerExternalWarning "Invalid numeric label"
             pure (At s 1)
  }

field :: { Located Field }
  : idField      { At (spanOf $1) $ FieldSymbol (valOf $1) }
  | numericField { At (spanOf $1) $ FieldInt    (valOf $1) }

const :: { Const }
  : INT    { At (spanOf $1) $ ConstInt (fst3 $ valOf $1) }
  | WORD   { At (spanOf $1) $ ConstWord (valOf $1) }
  | REAL   { At (spanOf $1) $ ConstReal (valOf $1) }
  | STRING { At (spanOf $1) $ ConstString (valOf $1) }
  | CHAR   { At (spanOf $1) $ ConstChar (valOf $1) }

{
spanOf :: Located a -> Span
spanOf = _located_span

valOf :: Located a -> a
valOf = _located_val

-- * Helpers

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

fieldToVid :: Located Field -> Vid
fieldToVid (At s (FieldSymbol sym)) = At s (Vid sym)
fieldToVid (At s (FieldInt n)) = At s (Vid (Text.pack (show n)))

makePatAs :: Pat -> Pat -> Parser Pat_
makePatAs p1 p2 =
  case fixopVar p1 of
    Just (fixop, var) -> pure $ PatLayered fixop var Nothing p2
    Nothing ->
      case p1 of
        At _ (PatConstraint p t) | Just (fixop, var) <- fixopVar p
          -> pure $ PatLayered fixop var (Just t) p2
    _ -> do
      withCurrentSpan (spanOf p1) $
        compilerExternalWarning "Must have variable to left of 'as' pattern"
      pure $ PatTuple []

  where
    fixopVar (At _ (PatFlatApp [At _ (PatVar fixop name)])) = Just (fixop, name)
    fixopVar _ = Nothing


-- * Pattern synonyms to improve working with nonterminals with values
pattern AtTokenID :: Located Text -> Token
pattern AtTokenID p <- (atTokenId -> Just p)
atTokenId (At s (Token.ID x)) = Just (At s x)
atTokenId _ = Nothing

pattern AtTokenSYMID :: Located Text -> Token
pattern AtTokenSYMID p <- (atTokSymId -> Just p)
atTokSymId (At s (Token.SYMID x)) = Just (At s x)
atTokSymId _ = Nothing

pattern AtTokenINT :: Located (Integer, NumRadix, Lazy.ByteString) -> Token
pattern AtTokenINT p <- (atTokInt -> Just p)
atTokInt (At s (Token.INT x r b)) = Just (At s (x, r, b))
atTokInt _ = Nothing

pattern AtTokenWORD :: Located Natural -> Token
pattern AtTokenWORD p <- (atTokWord -> Just p)
atTokWord (At s (Token.WORD x)) = Just (At s x)
atTokWord _ = Nothing

pattern AtTokenSTRING :: Located Text -> Token
pattern AtTokenSTRING p <- (atTokString -> Just p)
atTokString (At s (Token.STRING x)) = Just (At s x)
atTokString _ = Nothing

pattern AtTokenCHAR :: Located Char -> Token
pattern AtTokenCHAR p <- (atTokChar -> Just p)
atTokChar (At s (Token.CHAR x)) = Just (At s x)
atTokChar _ = Nothing

pattern AtTokenTYVARID :: Located Text -> Token
pattern AtTokenTYVARID p <- (atTokTyVarId -> Just p)
atTokTyVarId (At s (Token.TYVARID x)) = Just (At s x)
atTokTyVarId _ = Nothing

pattern AtTokenREAL :: Located Scientific -> Token
pattern AtTokenREAL p <- (atTokReal -> Just p)
atTokReal (At s (Token.REAL x)) = Just (At s x)
atTokReal _ = Nothing

-- | Happy primitive
parseError :: Token -> Parser a
parseError t =
  compilerInternalError ("Parse error on token: " <> Text.pack (show t)) "" CompilerLimitation
}
