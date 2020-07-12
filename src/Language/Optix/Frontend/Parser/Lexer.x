{
-- vim: set ft=haskell:
{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wno-unused-matches     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Optix.Frontend.Parser.Lexer
    ( lexToken
    , lexTokens
    , lexer
    ) where

import           Control.Monad              (unless, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.State.Strict (get, gets, modify')
import           Data.ByteString.Internal   (c2w)
import qualified Data.ByteString.Lazy       as Lazy (ByteString)
import qualified Data.ByteString.Lazy       as Lazy.ByteString
import qualified Data.ByteString.Lazy.Char8 as Lazy.ByteString.Char8
import           Data.Int                   (Int32, Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text.Encoding (decodeUtf8)
import           Data.Word                  (Word32, Word8)
-- import           Debug.Trace

import           Language.Optix.Frontend.Parser.Monad (AlexInput (..), Parser, ParserState (..))
import           Language.Optix.Frontend.Parser.Token (Token, Token_ (..))
import           Language.Optix.Utils.Located         (Located (At), SourcePos (..), Span (Span))
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
    -- Whitespace
    <0> $ws+     ;
    <0> @eol     ;

    -- Tokens
    <0> "("      { pureTok TokLParen    }
    <0> ")"      { pureTok TokRParen    }
    <0> ","      { pureTok TokComma     }
    <0> "->"     { pureTok TokArrow     }
    <0> ":"      { pureTok TokColon     }
    <0> ";"      { pureTok TokSemicolon }
    <0> "="      { pureTok TokEqual     }
    <0> "=>"     { pureTok TokDArrow    }
    <0> "_"      { pureTok TokWild      }
    <0> "{"      { pureTok TokLBrace    }
    <0> "}"      { pureTok TokRBrace    }
    <0> "."      { pureTok TokDot       }
    <0> "|"      { pureTok TokBar       }
    <0> "&"      { pureTok TokAmpersand }

    -- Keywords
    <0> and      { pureTok TokAnd    }
    <0> do       { pureTok TokDo     }
    <0> else     { pureTok TokElse   }
    <0> end      { pureTok TokEnd    }
    <0> false    { pureTok TokFalse  }
    <0> fn       { pureTok TokFn     }
    <0> fun      { pureTok TokFun    }
    <0> if       { pureTok TokIf     }
    <0> infixl   { pureTok TokInfixl }
    <0> infix    { pureTok TokInfix  }
    <0> infixr   { pureTok TokInfixr }
    <0> in       { pureTok TokIn     }
    <0> let      { pureTok TokLet    }
    <0> nonfix   { pureTok TokNonfix }
    <0> op       { pureTok TokOp     }
    <0> then     { pureTok TokThen   }
    <0> true     { pureTok TokTrue   }
    <0> val      { pureTok TokVal    }
    <0> _prim    { pureTok TokPrim   }

    -- Identifiers: alpha-numeric, symbols, type variables
    <0> @alphaNumId { textTok TokId      }
    <0> @symId      { textTok TokSymId   }
    <0> @tyVarId    { textTok TokTyVarId }

    -- Integers
    <0> @decNum       { numTok 10 0 TokInt            }
    <0> "~" @decNum   { numTok 10 1 (TokInt . negate) }
    <0> "0"  ("x"|"X") @hexNum { numTok 16 2 TokInt            }
    <0> "~0" ("x"|"X") @hexNum { numTok 16 3 (TokInt . negate) }
    <0> "0"  ("o"|"O") @octNum { numTok 8 2 TokInt             }
    <0> "~0" ("o"|"O") @octNum { numTok 8 3 (TokInt . negate)  }
    <0> "0"  ("b"|"B") @binNum { numTok 2 2 TokInt             }
    <0> "~0" ("b"|"B") @binNum { numTok 2 3 (TokInt . negate)  }

    -- Comments
    <0,comment> "(*" { beginComment }
    <comment> "*)" { endComment }
    <comment> .   ;
    <comment> $nl ;
    <0> "*)" { \span _ -> throwError $ "End comment at " ++ show span ++ " found outside of comment block" }

    -- Strings
    <0> \"      { startText }
    <text> \"   { endText }
    <text> \\a  { \_ _ -> addChar '\a' }
    <text> \\b  { \_ _ -> addChar '\b' }
    <text> \\t  { \_ _ -> addChar '\t' }
    <text> \\v  { \_ _ -> addChar '\v' }
    <text> \\f  { \_ _ -> addChar '\f' }
    <text> \\r  { \_ _ -> addChar '\r' }
    <text> \\n  { \_ _ -> addChar '\n' }
    <text> \\\\ { \_ _ -> addChar '\\' }
    <text> \\\" { \_ _ -> addChar '"' }
    <text> .    { \_ lbs -> addChar (Lazy.ByteString.Char8.head lbs) }

    -- Chars
    <0> "\#\""  { startChar }

{
-- * Helpers

-- | Produce a token without any info
pureTok :: Token_ -> Span -> Lazy.ByteString -> Parser Token
pureTok t span _ = pure $ At span t

-- | Produce a token that contains its text
textTok :: (Text -> Token_) -> Span -> Lazy.ByteString -> Parser Token
textTok t span lbs = pure $ At span (t $ lbsToText lbs)
    where
        lbsToText :: Lazy.ByteString -> Text
        lbsToText = Text.Encoding.decodeUtf8 . Lazy.ByteString.toStrict

-- | Produce a numeric token, with a given base and string offset
numTok :: Int32 -> Int64 -> (Int32 -> Token_) -> Span -> Lazy.ByteString -> Parser Token
numTok base offset t span lbs =
    let f :: Int32 -> Word8 -> Int32
        f acc c
          | c >= c2w '0' && c <= c2w '9' = base * acc + fromIntegral (c - c2w '0')
          | c >= c2w 'a' && c <= c2w 'f' = base * acc + fromIntegral (c - c2w 'a' + 10)
          | c >= c2w 'A' && c <= c2w 'F' = base * acc + fromIntegral (c - c2w 'A' + 10)
          | otherwise = acc
        acc = Lazy.ByteString.drop offset lbs
     in pure $ At span (t $ Lazy.ByteString.foldl' f 0 acc)

-- | Called when a '(*' is seen
beginComment :: Span -> Lazy.ByteString -> Parser Token
beginComment _ _ = do
    sourcePos <- gets (_alexInputSourcePos . _parserStateAlexInput)
    modify' $ \s -> s
        { _parserStateCommentStack = sourcePos : _parserStateCommentStack s
        , _parserStateLexState = comment
        }
    lexToken

-- | Called when a '*)' is seen
endComment :: Span -> Lazy.ByteString -> Parser Token
endComment _ _ = do
    commentStack <- gets _parserStateCommentStack
    case commentStack of
      [] -> error "Bug in lexer"
      (c:cs) -> do
          modify' $ \s -> s
              { _parserStateCommentStack = cs
              , _parserStateLexState = if null cs then 0 else comment
              }
          lexToken

startChar :: Span -> Lazy.ByteString -> Parser Token
startChar (Span _ sp _) _ = do
    modify' $ \s -> s
        { _parserStateTextBuffer = Text.empty
        , _parserStateTextLeft = sp
        , _parserStateLexState = text
        , _parserStateInChar = True
        }
    lexToken
startText :: Span -> Lazy.ByteString -> Parser Token
startText (Span _ sp _) _ = do
    modify' $ \s -> s
        { _parserStateTextBuffer = Text.empty
        , _parserStateTextLeft = sp
        , _parserStateLexState = text
        , _parserStateInText = True
        }
    lexToken
endText :: Span -> Lazy.ByteString -> Parser Token
endText (Span fileName _ sourcePos2) _ = do
    buf <- gets _parserStateTextBuffer
    sourcePos1 <- gets _parserStateTextLeft
    inChar <- gets _parserStateInChar
    modify' $ \s -> s
        { _parserStateTextBuffer = Text.empty
        , _parserStateLexState = 0
        , _parserStateInText = False
        , _parserStateInChar = False
        }
    let span = Span fileName sourcePos1 sourcePos2
    if inChar
       then if Text.length buf /= 1
               then throwError $ show span ++ "Character constant not of size 1"
               else pure $ At span $ TokChar (Text.head buf)
       else pure $ At span $ TokString (Text.reverse buf)
addChar :: Char -> Parser Token
addChar c = do
    modify' $ \s -> s
        { _parserStateTextBuffer = Text.cons c (_parserStateTextBuffer s)
        }
    lexToken

-- | Primitive used by Alex
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput sp bs) =
    case Lazy.ByteString.uncons bs of
      Nothing -> Nothing
      Just (c, bs') -> Just (c, AlexInput sp' bs')
          where
              (line, col)
                | c == c2w '\n' = (_sourcePosLine sp + 1, 1)
                | otherwise = (_sourcePosLine sp, _sourcePosCol sp + 1)
              sp' :: SourcePos
              sp' = SourcePos
                  { _sourcePosOffset = succ $ _sourcePosOffset sp
                  , _sourcePosLine = line
                  , _sourcePosCol = col
                  }

-- * Exported functions

-- | 'lexToken' in continuation-passing style for interop with Happy
lexer :: (Token -> Parser a) -> Parser a
lexer = (>>=) lexToken

-- | Lex many tokens until an error or EOF
lexTokens :: Parser [Token]
lexTokens = do
    t <- lexToken
    case t of
      At _ TokEOF -> pure [t]
      _ -> fmap (t:) lexTokens

-- | Lex a single token
lexToken :: Parser Token
lexToken = do
    ParserState
        { _parserStateAlexInput = input
        , _parserStateFileName = fileName
        , _parserStateLexState = lexState
        } <- get
    case alexScan input lexState of
      AlexEOF -> do
          commentStack <- gets _parserStateCommentStack
          unless (null commentStack) $
              throwError $ "Unclosed comment at " ++ show (head commentStack)
          inText <- gets _parserStateInText
          when inText $
              throwError "Unclosed string"
          inChar <- gets _parserStateInChar
          when inChar $
              throwError "Unclosed char"
          let sourcePos :: SourcePos
              sourcePos = _alexInputSourcePos input
              span :: Span
              span = Span fileName sourcePos sourcePos
          pure $ At span TokEOF
      AlexError (AlexInput sourcePos2 rest) -> do
          throwError "Lexical error"
      AlexSkip input' _ -> do
          modify' $ \s -> s { _parserStateAlexInput = input' }
          lexToken
      AlexToken input' _ action -> do
          modify' $ \s -> s { _parserStateAlexInput = input' }
          let sourcePos1 :: SourcePos
              sourcePos1 = _alexInputSourcePos input
              sourcePos2 :: SourcePos
              sourcePos2 = _alexInputSourcePos input'
              span :: Span
              span = Span fileName sourcePos1 sourcePos2
              len :: Word32
              len = _sourcePosOffset sourcePos2 - _sourcePosOffset sourcePos1
              tok :: Lazy.ByteString
              tok = Lazy.ByteString.take (fromIntegral len) (_alexInputInput input)
          action span tok
}
