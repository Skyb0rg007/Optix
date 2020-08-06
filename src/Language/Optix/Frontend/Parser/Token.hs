
module Language.Optix.Frontend.Parser.Token
    ( Token_ (..)
    , Token
    , NumRadix (..)
    , numRadix
    ) where

import qualified Data.ByteString.Lazy        as Lazy (ByteString)
import           Data.Scientific             (Scientific)
import           Data.Text                   (Text)
import           Language.Optix.Util.Located (Located)
import           Numeric.Natural             (Natural)

type Token = Located Token_

data Token_
    = EOF

    | HASH
    | HASHLBRACKET
    | LPAREN
    | RPAREN
    | COMMA
    | ARROW
    | DOT
    | COLON
    | SEMICOLON
    | EQUALOP
    | DARROW
    | LBRACKET
    | RBRACKET
    | WILD
    | LBRACE
    | BAR
    | RBRACE
    | AMPERSAND
    | ASTERISK

    | AND
    | ANDALSO
    | AS
    | CASE
    | DATATYPE
    | DO
    | ELSE
    | END
    | FN
    | FUN
    | IF
    | IN
    | INFIX
    | INFIXL
    | INFIXR
    | LET
    | LOCAL
    | NONFIX
    | OF
    | OP
    | ORELSE
    | THEN
    | TYPE
    | VAL
    | WHILE
    | WITHTYPE

    | ID !Text
    | TYVARID !Text
    | SYMID !Text

    | INT !Integer !NumRadix !Lazy.ByteString
    | WORD !Natural
    | REAL !Scientific
    | STRING !Text
    | CHAR !Char
    deriving (Show, Eq)

data NumRadix = HEX | BIN | DEC
    deriving (Show, Eq)

numRadix :: Num a => NumRadix -> a
numRadix BIN = 2
numRadix DEC = 10
numRadix HEX = 16

