{
-- vim: set ft=haskell ts=2 sw=2:
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Optix.Frontend.Parser.Parser
    ( parseProgram
    , parseExp
    ) where

import           Control.Monad.Error (throwError)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           Language.Optix.Frontend.Parser.Lexer (lexer)
import           Language.Optix.Frontend.Parser.Monad (Parser)
import           Language.Optix.Frontend.Parser.Token (Token, Token_ (..))
import           Language.Optix.Frontend.Syntax
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

    AND    { At $$ TokAnd    }
    DO     { At $$ TokDo     }
    ELSE   { At $$ TokElse   }
    END    { At $$ TokEnd    }
    FALSE  { At $$ TokFalse  }
    FN     { At $$ TokFn     }
    FUN    { At $$ TokFun    }
    IF     { At $$ TokIf     }
    IN     { At $$ TokIn     }
    INFIX  { At $$ TokInfix  }
    INFIXL { At $$ TokInfixl }
    INFIXR { At $$ TokInfixr }
    LET    { At $$ TokLet    }
    NONFIX { At $$ TokNonfix }
    OP     { At $$ TokOp     }
    PRIM   { At $$ TokPrim   }
    THEN   { At $$ TokThen   }
    TRUE   { At $$ TokTrue   }
    VAL    { At $$ TokVal    }

    ID      { AtTokId $$      }
    SYMID   { AtTokSymId $$   }
    TYVARID { AtTokTyVarId $$ }

    INT    { AtTokInt $$    }
    STRING { AtTokString $$ }
    -- CHAR   { AtTokChar $$ }

%name parseProgram fullProgram
%name parseExp     exp

%right AND
%right "->"
%right "|"
%right "&"
%right "=>"
%left ELSE
%left ":"

%%

fullProgram :: { Program } : program { $1 }

program :: { Program }
  : decs { Program $1 }

decs :: { [Decl] }
  : {- empty -}  { [] }
  | dec decs     { $1 : $2 }
  | ";" decs     { $2 }

dec :: { Decl }
  : VAL valbind { At ($1 <> spanOf $2) $ DeclVal (valOf $2) }
  | FUN funbind { At ($1 <> spanOf $2) $ DeclFun (valOf $2) }
  | DO exp      { At ($1 <> spanOf $2) $ DeclDo $2 }
  | fixity vids { At (spanOf $1 <> spanOf $2) $ DeclFixity $ FixityDecl (valOf $1) (valOf $2) }

funbind :: { Located (NonEmpty FunDecl) }
  : funClause { At (spanOf $1) (valOf $1 :| []) }
  | funClause AND funbind {
      At (spanOf $1 <> spanOf $3) $ NonEmpty.cons (valOf $1) (valOf $3)
  }

funClause :: { Located FunDecl }
  : apats constraint "=" exp {
      At (spanOf $1 <> spanOf $4) $ FunDecl (valOf $1) $2 $4
  }

valbind :: { Located (NonEmpty ValDecl) }
  : valClause { At (spanOf $1) $ valOf $1 :| [] }
  | valClause AND valbind {
      At (spanOf $1 <> spanOf $3) $ NonEmpty.cons (valOf $1) (valOf $3)
  }

valClause :: { Located ValDecl }
  : pat "=" exp {
        At (spanOf $1 <> spanOf $3) $ ValDecl $1 $3
  }

-- * Patterns

-- | Nonatomic pattern, a-la 'val'
pat :: { Pat }
  : cpat { $1 }

-- | Nonatomic pattern
cpat :: { Pat }
  -- : cpat as cpat { undefined }
  : vidNoEqual ":" ty    { At (spanOf $1 <> spanOf $3) $ PatVar (valOf $1) False (Just $3) }
  | OP vidNoEqual ":" ty { At ($1 <> spanOf $4) $ PatVar (valOf $2) True (Just $4) }
  | apat                 { $1 }

-- | List of atomic patterns, a-la 'fun'
apats :: { Located (NonEmpty Pat) }
  : apat       { At (spanOf $1) $ $1 :| [] }
  | apat apats { At (spanOf $1 <> spanOf $2) $ $1 `NonEmpty.cons` valOf $2 }

-- | Atomic pattern
apat :: { Pat }
  : vidNoEqual    { At (spanOf $1) $ PatVar (valOf $1) False Nothing }
  | OP vidNoEqual { At ($1 <> spanOf $2) $ PatVar (valOf $2) True Nothing }
  | "_"           { At $1 PatWild }
  | "(" pat ")"   { At ($1 <> $3) (valOf $2) }

-- * Fixity

fixity :: { Located Fixity }
  : INFIX        { At $1 $ Infix 0                          }
  | INFIX digit  { At ($1 <> spanOf $2) $ Infix (valOf $2)  }
  | INFIXL       { At $1 $ Infixl 0                         }
  | INFIXL digit { At ($1 <> spanOf $2) $ Infixl (valOf $2) }
  | INFIXR       { At $1 $ Infixr 0                         }
  | INFIXR digit { At ($1 <> spanOf $2) $ Infixr (valOf $2) }
  | NONFIX       { At $1 Nonfix                             }

digit :: { Located Int }
  : INT {%
      let At s n = $1
       in if 0 <= n && n <= 9
             then pure $ At s (fromIntegral n)
             else throwError $ "invalid number " <> show n <> " in infix declaration"
  }

-- * Identifiers

vids :: { Located (NonEmpty Text) }
  : vid      { At (spanOf $1) (valOf $1 :| []) }
  | vid vids { At (spanOf $1 <> spanOf $2) (NonEmpty.cons (valOf $1) (valOf $2)) }

vid :: { Located Text }
  : vidNoEqual { $1 }
  | vidEqual { $1 }

vidNoEqual :: { Located Text }
  : ID    { $1 }
  | SYMID { $1 }
vidEqual :: { Located Text }
  : "=" { At $1 "=" }

-- * Types

constraint :: { Maybe Ty }
  : {- empty -} { Nothing }
  | ":" ty      { Just $2 }

ty :: { Ty }
  : ty "->" ty {
        At (spanOf $1 <> spanOf $3) $ TyArrow $1 $3
  }
  | ty "|" ty {
        At (spanOf $1 <> spanOf $3) $ TyJoin $1 $3
  }
  | ty "&" ty {
        At (spanOf $1 <> spanOf $3) $ TyMeet $1 $3
  }
  | tynode { $1 }

tynode :: { Ty }
  : TYVARID { At (spanOf $1) $ TyVar (valOf $1) }
  | "{" "}" { At ($1 <> $2) $ TyRecord [] }
  | "{" tlabels "}" { At ($1 <> $3) $ TyRecord $2 }
  | "(" ty ")" { At ($1 <> $3) (valOf $2) }
  | ID {%
      case $1 of
        At s "int" -> pure $ At s TyInt
        At s "bool" -> pure $ At s TyBool
        At s "unit" -> pure $ At s TyUnit
        At s "top" -> pure $ At s TyTop
        At s "bot" -> pure $ At s TyBot
        At s x -> throwError $ show s <> ": Unknown type \"" <> Text.unpack x <> "\""
  }

tlabels :: { [(Text, Ty)] }
  : tlabel { [$1] }
  | tlabel tlabels { $1 : $2 }

tlabel :: { (Text, Ty) }
  : ID ":" ty { (valOf $1, $3) }

exp :: { Exp }
  : exp ":" ty {
        At (spanOf $1 <> spanOf $3) $ ExpConstraint $1 $3
  }
  | app_exp {
        At (spanOf $1) $ ExpFlatApp (valOf $1)
  }
  | FN pat "=>" exp { 
        At ($1 <> spanOf $4) $ ExpFn $2 $4
  }
  | IF exp THEN exp ELSE exp {
        At ($1 <> spanOf $6) $ ExpIf $2 $4 $6
  }

app_exp :: { Located (NonEmpty Exp) }
  : get_exp { At (spanOf $1) $ $1 :| [] }
  | get_exp app_exp {
         At (spanOf $1 <> spanOf $2) $ NonEmpty.cons $1 (valOf $2)
  }

get_exp :: { Exp }
  : aexp { $1 }
  | get_exp "." ID { At (spanOf $1 <> spanOf $3) $ ExpGet $1 (valOf $3) }

aexp :: { Exp }
  : vid    { At (spanOf $1) $ ExpVar (valOf $1) False }
  | OP vid { At ($1 <> spanOf $2) $ ExpVar (valOf $2) True }
  | INT { At (spanOf $1) $ ExpInt (fromIntegral $ valOf $1) }
  | TRUE { At $1 $ ExpBool True }
  | FALSE { At $1 $ ExpBool False }
  | "{" elabels "}" { At ($1 <> $3) $ ExpRecord $2 }
  | "{" "}" { At ($1 <> $2) $ ExpRecord [] }
  | "(" ")" { At ($1 <> $2) ExpUnit }
  | "(" exp ")" { At ($1 <> $3) (valOf $2) }
  | LET decs IN exp END { At ($1 <> $5) $ ExpLet $2 $4 }
  | PRIM STRING ":" ty ";" { At ($1 <> $5) $ ExpPrim (valOf $2) $4 }

elabels :: { [(Text, Exp)] }
  : elabel "," elabels { $1 : $3 }
  | elabel             { [$1] }

elabel :: { (Text, Exp) }
  : ID "=" exp { (valOf $1, $3) }

{
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

-- * Accessors for Located things
spanOf :: Located a -> Span
spanOf (At s _) = s

valOf :: Located a -> a
valOf (At _ x) = x

-- * Ran when there's a parse error
parseError :: Token -> Parser a
parseError t = throwError $ "Parser error with token: " ++ show t
}
