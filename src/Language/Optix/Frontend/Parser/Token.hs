
{-# OPTIONS_GHC -Wall #-}

module Language.Optix.Frontend.Parser.Token
    ( Token_ (..)
    , Token
    ) where

import           Data.Int  (Int32)
import           Data.Text (Text)

import           Language.Optix.Utils.Located (Located)

type Token = Located Token_
data Token_
    = TokEOF

    | TokLParen
    | TokRParen
    | TokComma
    | TokArrow
    | TokWild
    | TokColon
    | TokSemicolon
    | TokEqual
    | TokDArrow
    | TokLBrace
    | TokRBrace
    | TokBar
    | TokAmpersand

    | TokAnd
    | TokDo
    | TokDot
    | TokElse
    | TokEnd
    | TokFalse
    | TokFn
    | TokFun
    | TokIf
    | TokIn
    | TokInfix
    | TokInfixl
    | TokInfixr
    | TokLet
    | TokNonfix
    | TokOp
    | TokPrim
    | TokThen
    | TokTrue
    | TokVal

    | TokId !Text
    | TokTyVarId !Text
    | TokSymId !Text

    | TokInt !Int32
    | TokString !Text
    | TokChar !Char
    deriving (Show, Eq, Ord, Read)

