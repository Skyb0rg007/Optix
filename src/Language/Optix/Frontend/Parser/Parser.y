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

-- %name parseProgram fullProgram
-- %name parseExp     exp

%right AND
%right "->"
%right "|"
%right "&"
%right "=>"
%left ELSE
%left ":"

%%

asdf : { undefined }

{

}
