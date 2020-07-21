{
-- vim: set ft=haskell:

{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wno-unused-matches     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Optix.Frontend.Parser.Lexer
    ( lexToken
    , lexer
    ) where

}

$ws = [\t\v\f\ ]
$cr = [\r]
$nl = [\n]
@eol = $cr $nl | $nl | $cr

$alphaNum = [a-zA-Z0-9'_]
@alphaNumId = [a-zA-Z] $alphaNum*

$sym = [ \- \! \% \& \$ \# \+ \/ \: \< \= \> \? \@ \\ \~ \` \^ \| \*]
@symId = $sym+

@tyVarId = "'" $alphaNum*

$decDigit = [0-9]
@decNum = $decDigit+
$hexDigit = [0-9a-fA-F]
@hexNum = $hexDigit+
$octDigit = [0-7]
@octNum = $octDigit+
$binDigit = [0-1]
@binNum = $binDigit+

tokens :-
    <0> $ws+   ;
    <0> @eol   ;

{
lexer = undefined
lexToken = undefined
}

