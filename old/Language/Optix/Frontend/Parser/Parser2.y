{
-- vim: set ft=haskell ts=2 sw=2:
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Optix.Frontend.Parser.Parser2 where

import           Control.Monad.Error (throwError)
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           Language.Optix.Frontend.Parser.Lexer (lexer)
import           Language.Optix.Frontend.Parser.Monad (Parser)
import           Language.Optix.Frontend.Parser.Token (Token, Token_ (..))
import           Language.Optix.Frontend.AST
import           Language.Optix.Utils.Located         (Located (At), Span)
import           Language.Optix.Utils.Pretty          (tshow)
}

%monad { Parser } { (>>=) } { pure }
%lexer { lexer } { At _ TokEOF }
%tokentype { Token }
%error { parseError }

%token
    "("  { At $$ TokLParen    }
    ")"  { At $$ TokRParen    }
    ","  { At $$ TokComma     }
    "->" { At $$ TokArrow     }
    "_"  { At $$ TokWild      }
    ":"  { At $$ TokColon     }
    ";"  { At $$ TokSemicolon }
    "="  { At $$ TokEqual     }
    "=>" { At $$ TokDArrow    }
    "{"  { At $$ TokLBrace    }
    "}"  { At $$ TokRBrace    }
    "."  { At $$ TokDot       }
    "|"  { At $$ TokBar       }
    "&"  { At $$ TokAmpersand }

    AND     { At $$ TokAnd     }
    ANDALSO { At $$ TokAndAlso }
    AS      { At $$ TokAs      }
    DO      { At $$ TokDo      }
    ELSE    { At $$ TokElse    }
    END     { At $$ TokEnd     }
    FALSE   { At $$ TokFalse   }
    FN      { At $$ TokFn      }
    FUN     { At $$ TokFun     }
    IF      { At $$ TokIf      }
    IN      { At $$ TokIn      }
    INFIX   { At $$ TokInfix   }
    INFIXL  { At $$ TokInfixl  }
    INFIXR  { At $$ TokInfixr  }
    LET     { At $$ TokLet     }
    NONFIX  { At $$ TokNonfix  }
    OP      { At $$ TokOp      }
    ORELSE  { At $$ TokOrElse  }
    PRIM    { At $$ TokPrim    }
    THEN    { At $$ TokThen    }
    TRUE    { At $$ TokTrue    }
    VAL     { At $$ TokVal     }

    ID      { AtTokId $$      }
    SYMID   { AtTokSymId $$   }
    TYVARID { AtTokTyVarId $$ }

    INT    { AtTokInt    $$ }
    STRING { AtTokString $$ }
    CHAR   { AtTokChar   $$ }
    REAL   { AtTokReal   $$ }

-- %name parseProgram fullProgram
-- %name parseExp     exp

%right AND
%right "->"
%right "|"
%right "&"
%right "=>"
%left DO
%left ELSE
%left ORELSE
%left ANDALSO
%right AS
%left ":"

%name parseProgram program
%name parseExp exp

%%

program :: { Program }
  : decs { Program $1 }

--
-- * Declarations
--
decs :: { [Dec] }
  : {- empty -}  { [] }
  | dec decs     { $1 : $2 }
  | ";" decs     { $2 }

dec :: { Dec }
  : VAL valbind { At ($1 <> spanOf $2) $ DecVal (valOf $2) }
  | DO exp      { At ($1 <> spanOf $2) $ DecDo $2 }
  | FUN funs    { At ($1 <> spanOf $2) $ DecFun (valOf $2) }
  | fixity vids { At (spanOf $1 <> spanOf $2) $ DecFixity (valOf $1) (valOf $2) }

-- DecVal
valbind :: { Located [ValClause] }
  : valClause { At (spanOf $1) [$1] }
  | valClause AND valbind { At (spanOf $1 <> spanOf $3) $ $1 : valOf $3 }

valClause :: { ValClause }
  : pat "=" exp { At (spanOf $1 <> spanOf $3) $ ValClause $1 $3 }

-- DecFun
funs :: { Located [Located [FunClause]] }
  : funClauses { At (spanOf $1) [$1] }
  | funClauses AND funs { At (spanOf $1 <> spanOf $3) ($1:valOf $3) }

funClauses :: { Located [FunClause] }
  : funClause { At (spanOf $1) [$1] }
  | funClause "|" funClauses { At (spanOf $1 <> spanOf $3) ($1:valOf $3) }

funClause :: { FunClause }
  : apats constraint "=" exp { At (spanOf $1 <> spanOf $4) $ FunClause (valOf $1) $2 $4 }

-- DecFixity
fixity :: { Located Fixity }
  : INFIX        { At $1 $ Infix Nothing                           }
  | INFIX digit  { At ($1 <> spanOf $2) $ Infix (Just $ valOf $2)  }
  | INFIXL       { At $1 $ InfixL Nothing                          }
  | INFIXL digit { At ($1 <> spanOf $2) $ InfixL (Just $ valOf $2) }
  | INFIXR       { At $1 $ InfixR Nothing                          }
  | INFIXR digit { At ($1 <> spanOf $2) $ InfixR (Just $ valOf $2) }
  | NONFIX       { At $1 Nonfix                                    }

digit :: { Located Int }
  : INT {%
      let At s n = $1
       in if 0 <= n && n <= 9
             then pure $ At s (fromIntegral n)
             else throwError $ "invalid number " <> show n <> " in infix declaration"
  }

--
-- * Patterns
--

pat :: { Pat }
  : cpat { $1 }

cpat :: { Pat }
  : cpat AS cpat {% patAs $1 $3 }
  | cpat ":" ty  { At (spanOf $1 <> spanOf $3) $ PatConstraint $1 $3 }
  | apats        { At (spanOf $1) $ PatFlatApp (valOf $1) }

apats :: { Located [Pat] }
  : apat       { At (spanOf $1) [$1] }
  | apat apats { At (spanOf $1 <> spanOf $2) $ $1 : valOf $2 }

apat :: { Pat }
  : vidNoEqual  { At (spanOf $1) $ PatVar NoOp (valOf $1) }
  | OP vid      { At ($1 <> spanOf $2) $ PatVar Op (valOf $2) }
  | const       { At (spanOf $1) $ PatConst (valOf $1) }
  | "_"         { At $1 PatWild }
  | "(" pat ")" { At ($1 <> $3) $ PatParen $2 }
  | "(" ")"     { At ($1 <> $2) $ PatRecord [] }
  | "{" "}"     { At ($1 <> $2) $ PatRecord [] }
  | "{" patitems "}" { At ($1 <> $3) $ PatRecord $2 }

patitems :: { [Located (Text, PatItem)] }
  : patitem "," patitems { $1:$3 }
  | patitem              { [$1]  }

patitem :: { Located (Text, PatItem) }
  : label "=" pat {
      At (spanOf $1 <> spanOf $3) (valOf $1, PatItemField $3)
  }
  | vidNoEqual constraint AS pat {
      At (spanOf $1 <> spanOf $4) $ (valOf $1, PatItemVid (valOf $1) $2 (Just $4))
  }
  | vidNoEqual {
      At (spanOf $1) $ (valOf $1, PatItemVid (valOf $1) Nothing Nothing)
  }
  | vidNoEqual ":" ty {
      At (spanOf $1 <> spanOf $3) $ (valOf $1, PatItemVid (valOf $1) (Just $3) Nothing)
  }

-- * Identifiers

vids :: { Located [Text] }
  : vid      { At (spanOf $1) [valOf $1] }
  | vid vids { At (spanOf $1 <> spanOf $2) (valOf $1 : valOf $2) }

vid :: { Located Text }
  : vidNoEqual { $1 }
  | vidEqual   { $1 }

vidNoEqual :: { Located Text }
  : ID    { $1 }
  | SYMID { $1 }
vidEqual :: { Located Text }
  : "=" { At $1 "=" }

--
-- * Types
--

constraint :: { Maybe Ty }
  : {- empty -} { Nothing }
  | ":" ty      { Just $2 }

ty :: { Ty }
  : ty "->" ty { At (spanOf $1 <> spanOf $3) $ TyCon "->" [$1, $3] }
  | ty "|" ty  { At (spanOf $1 <> spanOf $3) $ TyJoin $1 $3 }
  | ty "&" ty  { At (spanOf $1 <> spanOf $3) $ TyMeet $1 $3 }
  | ty1        { $1 }

ty1 :: { Ty }
  : TYVARID         { At (spanOf $1) $ TyVar (valOf $1) }
  | "{" tlabels "}" { At ($1 <> $3) $ TyRecord $2       }
  | "{" "}"         { At ($1 <> $2) $ TyRecord []       }
  | "(" ty0_pc ")" tycon { At ($1 <> spanOf $4) $ TyCon (valOf $4) $2 }
  | "(" ty ")"      { At ($1 <> $3) $ TyParen $2        }
  | ty1 tycon       { At (spanOf $1 <> spanOf $2) $ TyCon (valOf $2) [$1] }
  | tycon           { At (spanOf $1) $ TyCon (valOf $1) [] }

tycon :: { Located Text }
  : vidNoEqual { $1 }

ty0_pc :: { [Ty] }
  : ty "," ty { [$1, $3] }
  | ty "," ty0_pc { $1 : $3 }

tlabels :: { [Located (Text, Ty)] }
  : tlabel { [$1] }
  | tlabel tlabels { $1 : $2 }

tlabel :: { Located (Text, Ty) }
  : label ":" ty { At (spanOf $1 <> spanOf $3) (valOf $1, $3) }

--
-- * Expressions
--

exp :: { Exp }
  : exp ORELSE exp { At (spanOf $1 <> spanOf $3) $ ExpOrElse $1 $3 }
  | exp ANDALSO exp { At (spanOf $1 <> spanOf $3) $ ExpAndAlso $1 $3 }
  | exp ":" ty {
        At (spanOf $1 <> spanOf $3) $ ExpConstraint $1 $3
  }
  | app_exp {
        At (spanOf $1) $ ExpFlatApp (valOf $1)
  }
  | FN fnmatch { At ($1 <> spanOf $2) $ ExpFn (valOf $2) }
  | IF exp THEN exp ELSE exp {
        At ($1 <> spanOf $6) $ ExpIf $2 $4 $6
  }

fnmatch :: { Located [Located (Pat, Exp)] }
  : fnrule { At (spanOf $1) [$1] }
  | fnrule "|" fnmatch { At (spanOf $1 <> spanOf $3) ($1 : valOf $3) }

fnrule :: { Located (Pat, Exp) }
  : pat "=>" exp { At (spanOf $1 <> spanOf $3) ($1, $3) }

app_exp :: { Located [Exp] }
  : get_exp { At (spanOf $1) $ [$1] }
  | get_exp app_exp { At (spanOf $1 <> spanOf $2) ($1 : valOf $2) }

get_exp :: { Exp }
  : aexp { $1 }
  | get_exp "." label { At (spanOf $1 <> spanOf $3) $ ExpGet $1 (valOf $3) }

aexp :: { Exp }
  : vid    { At (spanOf $1) $ ExpVar NoOp (valOf $1) }
  | OP vid { At ($1 <> spanOf $2) $ ExpVar Op (valOf $2) }
  | const  { At (spanOf $1) $ ExpConst (valOf $1) }
  | "{" elabels "}" { At ($1 <> $3) $ ExpRecord $2 }
  | "{" "}" { At ($1 <> $2) $ ExpRecord [] }
  | "(" ")" { At ($1 <> $2) $ ExpRecord [] }
  | "(" exp_ps ")" {
      At ($1 <> $3) $
        case $2 of
          [e] -> ExpParen e
          _ -> ExpSequence $2
  }
  | LET decs IN exp END { At ($1 <> $5) $ ExpLet $2 $4 }
  | PRIM STRING ":" ty ";" { At ($1 <> $5) $ ExpPrim $ At ($1 <> $5) $ Prim (valOf $2) $4 }

exp_ps :: { [Exp] }
  : exp            { [$1] }
  | exp ";" exp_ps { $1 : $3 }

elabels :: { [Located (Text, Exp)] }
  : elabel "," elabels { $1 : $3 }
  | elabel             { [$1] }

elabel :: { Located (Text, Exp) }
  : label "=" exp { At (spanOf $1 <> spanOf $3) (valOf $1, $3) }

--
-- * Atoms
--

const :: { Located Const }
  : INT { fmap ConstInt $1 }
  | REAL { fmap ConstReal $1 }
  | STRING { fmap ConstString $1 }
  | CHAR { fmap ConstChar $1 }
  | TRUE { At $1 (ConstBool True) }
  | FALSE { At $1 (ConstBool False) }

label :: { Located Text }
  : vidNoEqual { $1 }

{

patAs :: Pat -> Pat -> Parser Pat
patAs (At s1 p1) (At s2 p2) =
  case fixopVar p1 of
    Just (op, x) -> pure $ At (s1 <> s2) $ PatAs op x Nothing (At s2 p2)
    Nothing | PatConstraint (At _ p) t <- p1, Just (op, x) <- fixopVar p ->
      pure $ At (s1 <> s2) $ PatAs op x (Just t) (At s2 p2)
    Nothing -> throwError "Must have variable to left in as pattern"
  where
    fixopVar (PatFlatApp [At _ (PatVar op x)]) = Just (op, x)
    fixopVar _ = Nothing

-- * Pattern synonyms to improve working with nonterminals with values
pattern AtTokId :: Located Text -> Token
pattern AtTokId p <- (atTokId -> Just p)
atTokId (At s (TokId x)) = Just (At s x)
atTokId _ = Nothing

pattern AtTokSymId :: Located Text -> Token
pattern AtTokSymId p <- (atTokSymId -> Just p)
atTokSymId (At s (TokSymId x)) = Just (At s x)
atTokSymId _ = Nothing

pattern AtTokInt :: Located Integer -> Token
pattern AtTokInt p <- (atTokInt -> Just p)
atTokInt (At s (TokInt x)) = Just (At s x)
atTokInt _ = Nothing

pattern AtTokString :: Located Text -> Token
pattern AtTokString p <- (atTokString -> Just p)
atTokString (At s (TokString x)) = Just (At s x)
atTokString _ = Nothing

pattern AtTokChar :: Located Char -> Token
pattern AtTokChar p <- (atTokChar -> Just p)
atTokChar (At s (TokChar x)) = Just (At s x)
atTokChar _ = Nothing

pattern AtTokTyVarId :: Located Text -> Token
pattern AtTokTyVarId p <- (atTokTyVarId -> Just p)
atTokTyVarId (At s (TokTyVarId x)) = Just (At s x)
atTokTyVarId _ = Nothing

pattern AtTokReal :: Located Scientific -> Token
pattern AtTokReal p <- (atTokReal -> Just p)
atTokReal (At s (TokReal x)) = Just (At s x)
atTokReal _ = Nothing

-- * Accessors for Located things
spanOf :: Located a -> Span
spanOf (At s _) = s

valOf :: Located a -> a
valOf (At _ x) = x

-- * Ran when there's a parse error
parseError :: Token -> Parser a
parseError t = throwError $ "Parser error with token: " ++ show t
}
