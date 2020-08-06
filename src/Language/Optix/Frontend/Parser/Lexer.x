{
-- vim: set ft=haskell:

-- Note: this is translated from the MLton project source code:
-- https://github.com/MLton/mlton/blob/master/mlton/front-end/ml.lex

module Language.Optix.Frontend.Parser.Lexer
    ( lexToken
    , lexer
    ) where

import           Control.Monad
import           Debug.Trace
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.ByteString.Internal             (c2w, w2c)
import qualified Data.ByteString.Lazy                 as Lazy (ByteString)
import qualified Data.ByteString.Lazy                 as Lazy.ByteString
import qualified Data.ByteString.Lazy.Char8           as Lazy.ByteString.Char8
import           Data.Char
import           Data.Function                        ((&))
import           Data.Int                             (Int64)
import           Data.Maybe
import           Data.Scientific                      (scientific)
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text.Encoding
import           Data.Void                            (vacuous)
import           Data.Word                            (Word32, Word8)
import           Language.Optix.Frontend.Parser.Monad
import           Language.Optix.Frontend.Parser.Token (Token)
import qualified Language.Optix.Frontend.Parser.Token as Token
import           Language.Optix.Util.Error
import           Language.Optix.Util.Located
import           Language.Optix.Util.Pretty
import           Numeric.Natural
import           Text.Read                            (readMaybe)
}

$ws = [\t\v\f\ ]
$cr = [\r]
$nl = [\n]
-- Removed '$cr' due to Alex limitations
@eol = $cr $nl | $nl

$alphanum = [a-zA-Z0-9'_]
@alphanumId = [a-zA-Z] $alphanum*

$sym = [\! \% \& \$ \# \+ \- \/ \: \< \= \> \? \@ \\ \~ \` \^ \| \*]
@symId = $sym+

@tyvarId = "'" $alphanum*
-- No module system
-- @longSymId = (@alphanumId ".")+ @symId
-- @longAlphanumId = (@alphanumId ".")+ @alphanumId

$decDigit = [0-9]
@decnum = $decDigit ("_"* $decDigit)*
$hexDigit = [0-9a-fA-F]
@hexnum = $hexDigit ("_"* $hexDigit)*
$binDigit = [0-1]
@binnum = $binDigit ("_"* $binDigit)*
@frac = "." @decnum
@exp = [eE] "~"? @decnum
@real = "~"? (@decnum @frac? @exp | @decnum @frac @exp?)

tokens :-
    <0> $ws+   ;
    <0> @eol   ;

    -- MLton primitives / extensions
    -- <0> "_address" { pureTok Token.ADDRESS }
    -- <0> "_build_const" { pureTok Token.BUILD_CONST }
    -- <0> "_command_line_const" { pureTok Token.COMMAND_LINE_CONST }
    -- <0> "_const" { pureTok Token.CONST }
    -- <0> "_export" { pureTok Token.EXPORT }
    -- <0> "_import" { pureTok Token.CONST }
    -- <0> "_overload" { pureTok Token.OVERLOAD }
    -- <0> "_prim" { pureTok Token.PRIM }
    -- <0> "_symol" { pureTok Token.SYMBOL }
    
    -- Reserved symbols
    -- Not supporting:
    --   :> - no modules
    --   ... - not needed due to subtyping
    <0> "#"  { pureTok Token.HASH      }
    <0> "#[" { pureTok Token.HASHLBRACKET }
    <0> "("  { pureTok Token.LPAREN    }
    <0> ")"  { pureTok Token.RPAREN    }
    <0> ","  { pureTok Token.COMMA     }
    <0> "->" { pureTok Token.ARROW     }
    <0> "."  { pureTok Token.DOT       } -- Added
    -- <0> "..." { pureTok Token.DOTDOTDOT }
    <0> ":"  { pureTok Token.COLON     }
    -- <0> ":>" { pureTok Token.COLONGT }
    <0> ";"  { pureTok Token.SEMICOLON }
    <0> "="  { pureTok Token.EQUALOP   }
    <0> "=>" { pureTok Token.DARROW    }
    <0> "["  { pureTok Token.LBRACKET  }
    <0> "]"  { pureTok Token.RBRACKET  }
    <0> "_"  { pureTok Token.WILD      }
    <0> "{"  { pureTok Token.LBRACE    }
    <0> "|"  { pureTok Token.BAR       }
    <0> "}"  { pureTok Token.RBRACE    }

    -- Reserved Keywords
    -- Not supporting:
    --  abstype - depreciated
    --  modules - too much for now
    --  exception/handle/raise - no exceptions
    --  rec - no val rec
    -- <0> "abstype" { pureTok Token.ABSTYPE }
    <0> "and"      { pureTok Token.AND      }
    <0> "andalso"  { pureTok Token.ANDALSO  }
    <0> "as"       { pureTok Token.AS       }
    <0> "case"     { pureTok Token.CASE     }
    <0> "datatype" { pureTok Token.DATATYPE }
    <0> "do"       { pureTok Token.DO       }
    <0> "else"     { pureTok Token.ELSE     }
    <0> "end"      { pureTok Token.END      }
    -- <0> "eqtype" { pureTok Token.EQTYPE }
    -- <0> "exception" { pureTok Token.EXCEPTION }
    <0> "fn"  { pureTok Token.FN  }
    <0> "fun" { pureTok Token.FUN }
    -- <0> "functor" { pureTok Token.FUNCTOR }
    -- <0> "handle" { pureTok Token.HANDLE }
    <0> "if" { pureTok Token.IF }
    <0> "in" { pureTok Token.IN }
    -- <0> "include" { pureTok Token.INCLUDE }
    <0> "infix"  { pureTok Token.INFIX  }
    <0> "infixl" { pureTok Token.INFIXL } -- Added
    <0> "infixr" { pureTok Token.INFIXR }
    <0> "let"    { pureTok Token.LET    }
    <0> "local"  { pureTok Token.LOCAL  }
    <0> "nonfix" { pureTok Token.NONFIX }
    <0> "of"     { pureTok Token.OF     }
    <0> "op"     { pureTok Token.OP     }
    -- <0> "open" { pureTok Token.OPEN }
    <0> "orelse" { pureTok Token.ORELSE }
    -- <0> "raise" { pureTok Token.RAISE }
    -- <0> "rec" { pureTok Token.REC }
    -- <0> "sharing" { pureTok Token.SHARING }
    -- <0> "sig" { pureTok Token.SIG }
    -- <0> "signature" { pureTok Token.SIGNATURE }
    -- <0> "struct" { pureTok Token.STRUCT }
    -- <0> "structure" { pureTok Token.STRUCTURE }
    <0> "then" { pureTok Token.THEN }
    <0> "type" { pureTok Token.TYPE }
    <0> "val"  { pureTok Token.VAL  }
    -- <0> "where" { pureTok Token.WHERE }
    <0> "while" { pureTok Token.WHILE }
    -- <0> "with" { pureTok Token.WITH }
    <0> "withtype" { pureTok Token.WITHTYPE }

    <0> @alphanumId { textTok Token.ID      }
    <0> @tyvarId    { textTok Token.TYVARID }
    <0> @symId      { \span lbs ->
        case lbs of
          "*" -> pure $ At span Token.ASTERISK
          "&" -> pure $ At span Token.AMPERSAND
          _ -> textTok Token.SYMID span lbs }

    <0> @real        { realTok }
    <0> @decnum      { intTok  NumTokOpts { _numTokOpts_drop = 0, _numTokOpts_negate = False, _numTokOpts_radix = Token.DEC } }
    <0> \~   @decnum { intTok  NumTokOpts { _numTokOpts_drop = 1, _numTokOpts_negate = True,  _numTokOpts_radix = Token.DEC } }
    <0> 0x   @hexnum { intTok  NumTokOpts { _numTokOpts_drop = 2, _numTokOpts_negate = False, _numTokOpts_radix = Token.HEX } }
    <0> \~0x @hexnum { intTok  NumTokOpts { _numTokOpts_drop = 3, _numTokOpts_negate = True,  _numTokOpts_radix = Token.HEX } }
    <0> 0b   @binnum { intTok  NumTokOpts { _numTokOpts_drop = 2, _numTokOpts_negate = False, _numTokOpts_radix = Token.BIN } }
    <0> \~0b @binnum { intTok  NumTokOpts { _numTokOpts_drop = 3, _numTokOpts_negate = True,  _numTokOpts_radix = Token.BIN } }
    <0> 0w   @decnum { wordTok NumTokOpts { _numTokOpts_drop = 2, _numTokOpts_negate = False, _numTokOpts_radix = Token.DEC } }
    <0> 0wx  @hexnum { wordTok NumTokOpts { _numTokOpts_drop = 3, _numTokOpts_negate = False, _numTokOpts_radix = Token.HEX } }
    <0> 0wb  @binnum { wordTok NumTokOpts { _numTokOpts_drop = 3, _numTokOpts_negate = False, _numTokOpts_radix = Token.BIN } }

    <0> \"  { startString }
    <0> \#\" { startChar }

    <textState> \" { finishText }
    <textState> " "|\!|[\035-\091]|[\093-\126]               { \_ lbs -> addText (lbsToText lbs) >> lexToken }
    <textState> [\192-\223]|[\128-\191]                      { \_ lbs -> addText (lbsToText lbs) >> lexToken }
    <textState> [\224-\239][\128-\191][\128-\191]            { \_ lbs -> addText (lbsToText lbs) >> lexToken }
    <textState> [\240-\247][\128-\191][\128-\191][\128-\191] { \_ lbs -> addText (lbsToText lbs) >> lexToken }
    <textState> \\a { \_ _ -> addText "\a" >> lexToken }
    <textState> \\b { \_ _ -> addText "\b" >> lexToken }
    <textState> \\t { \_ _ -> addText "\t" >> lexToken }
    <textState> \\n { \_ _ -> addText "\n" >> lexToken }
    <textState> \\v { \_ _ -> addText "\v" >> lexToken }
    <textState> \\f { \_ _ -> addText "\f" >> lexToken }
    <textState> \\r { \_ _ -> addText "\r" >> lexToken }
    <textState> \\\^ [\@-_] { \_ lbs ->
        let go c = Text.singleton (w2c (c2w c - c2w '@'))
         in addText (go (Lazy.ByteString.Char8.index lbs 2)) >> lexToken
    }
    <textState> \\\^. { \_ _ -> do
        compilerExternalWarning "Illegal control escape in text constant; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        lexToken
    }
    <textState> \\ [0-9]{3}      { \_ _ -> compilerInternalWarning "Decimal escapes" "" CompilerLimitation >> lexToken }
    <textState> \\u $hexDigit{4} { \_ _ -> compilerInternalWarning "Hex escapes" "" CompilerLimitation >> lexToken }
    <textState> \\U $hexDigit{8} { \_ _ -> compilerInternalWarning "\\U escapes" "" CompilerLimitation >> lexToken }
    <textState> \\\"   { \_ _ -> addText "\"" >> lexToken }
    <textState> \\\\ { \_ _ -> addText "\\" >> lexToken }
    <textState> \\ $ws+ { \_ _ -> begin textFmtState >> lexToken }
    <textState> \\ @eol { \_ _ -> begin textFmtState >> lexToken }
    <textState> . { \_ _ -> compilerExternalWarning "Illegal character in text constant" >> lexToken }

    <textFmtState> $ws+ ;
    <textFmtState> @eol { \_ _ -> internalError "TEXT_FMT" CompilerLimitation }
    <textFmtState> "\\" { \_ _ -> internalError "TEXT_FMT" CompilerLimitation }
    <textFmtState> .    { \_ _ -> internalError "TEXT_FMT" CompilerLimitation {- ERROR -} }

    -- Comments
    <0> "(*)" { \span _ -> startComment (begin 0) span >> begin lineCommentState  >> lexToken }
    <0> "(*"  { \span _ -> startComment (begin 0) span >> begin blockCommentState >> lexToken }
    <lineCommentState> @eol { \_ _ -> finishComment >> lexToken }
    <lineCommentState> .    ;
    <blockCommentState> "(*)" { \span _ -> startComment (begin blockCommentState) span >> begin lineCommentState >> lexToken }
    <blockCommentState> "(*"  { \span _ -> startComment (begin blockCommentState) span >> begin blockCommentState >> lexToken }
    <blockCommentState> "*)"  { \_ _ -> finishComment >> lexToken }
    <blockCommentState> @eol  ;
    <blockCommentState> .     ;

    <0> . { \span lbs -> do
        compilerExternalWarning ("Illegal token \"" <> vacuous (pretty (lbsToText lbs)) <> "\"")
        lexToken
    }
{
-- | CPS-ed 'lexToken', used in the generated Happy parser
lexer :: (Token -> Parser a) -> Parser a
lexer = (>>=) lexToken

-- | Lex a single token
lexToken :: Parser Token
lexToken = do
    s <- get
    let input = _parserState_alexInput s
        lexState = _parserState_lexState s
    case alexScan input lexState of
      AlexEOF -> do
          gets _parserState_inComment >>= flip when (compilerExternalWarning "Unclosed comment at end of file")
          gets _parserState_inText    >>= flip when (compilerExternalWarning "Unclosed text constant at end of file")
          span <- getCurrentSpan
          pure $ At span Token.EOF
      AlexError input'@(AlexInput sourcePos2 rest) ->
          compilerInternalError ("AlexError: input' = " <> Text.pack (show input')) "" CompilerLimitation
      AlexSkip input' _ -> do
          modify' $ \s -> s { _parserState_alexInput = input' }
          lexToken
      AlexToken input' _ action -> do
          modify' $ \s -> s { _parserState_alexInput = input' }
          let sourcePos1 = _alexInput_sourcePos input
              sourcePos2 = _alexInput_sourcePos input'
              span = Span sourcePos1 sourcePos2
              len = _sourcePos_offset sourcePos2 - _sourcePos_offset sourcePos1
              tok = Lazy.ByteString.take (fromIntegral len) (_alexInput_input input)
          withCurrentSpan span $
              action span tok

-- Parse the given token
pureTok :: Token.Token_ -> Span -> Lazy.ByteString -> Parser Token
pureTok t span _ = pure $ At span t

-- Parse a token with a Text argument
textTok :: (Text -> Token.Token_) -> Span -> Lazy.ByteString -> Parser Token
textTok t span lbs = pure $ At span $ t $ lbsToText lbs

-- Change the lexState
begin :: Int -> Parser ()
begin lexState = modify' $ \s -> s { _parserState_lexState = lexState }

-- Convert a lazy ByteString to strict Text
lbsToText :: Lazy.ByteString -> Text
lbsToText lbs = 
    case Text.Encoding.decodeUtf8' (Lazy.ByteString.toStrict lbs) of
      Left err -> internalError ("lbsToText: " <> Text.pack (show err)) CompilerBug
      Right txt -> txt

-- Parse a REAL token
realTok :: Span -> Lazy.ByteString -> Parser Token
realTok span lbs =
    let lbs' = lbs
            & Lazy.ByteString.Char8.filter (/= '_')
            & Lazy.ByteString.Char8.map (\c -> if c == '~' then '-' else c)
     in case readMaybe (Lazy.ByteString.Char8.unpack lbs') of
          Nothing -> do
              compilerExternalWarning "Error reading real literal"
              pure $ At span $ Token.REAL 0
          -- TODO: detect overflow vs other errors
          Just s -> pure $ At span $ Token.REAL s

-- Options for parsing numbers
data NumTokOpts = NumTokOpts
    { _numTokOpts_drop   :: Int64
    , _numTokOpts_negate :: Bool
    , _numTokOpts_radix  :: Token.NumRadix
    }

-- Common routine between 'intTok' and 'wordTok'
parseNum :: NumTokOpts -> Lazy.ByteString -> Natural
parseNum (NumTokOpts drop _neg radix) lbs =
    Lazy.ByteString.Char8.foldl' f 0 $ Lazy.ByteString.drop drop lbs
        where
            base :: Natural
            base = Token.numRadix radix
            f :: Natural -> Char -> Natural
            f n '_' = n
            f n c = n * base + fromIntegral (digitVal c)
            digitVal :: Char -> Word8
            digitVal c
              | '0' <= c && c <= '9' = c2w c - c2w '0'
              | 'a' <= c && c <= 'f' = c2w c - c2w 'a' + 10
              | 'A' <= c && c <= 'F' = c2w c - c2w 'F' + 10
              | otherwise =
                  internalError
                  ("digitVal called on non-digit " <> Text.pack (show c))
                  CompilerBug

-- Parse an INT token
intTok :: NumTokOpts -> Span -> Lazy.ByteString -> Parser Token
intTok opts@(NumTokOpts _ neg radix) span lbs =
    pure $ At span $ (\n -> Token.INT n radix lbs) $
        (if neg then negate else id) $ toInteger $ parseNum opts lbs

-- Parse a WORD token
wordTok :: NumTokOpts -> Span -> Lazy.ByteString -> Parser Token
wordTok opts span lbs =
    pure $ At span $ Token.WORD (parseNum opts lbs)

startText :: Parser Token -> Span -> Lazy.ByteString -> Parser Token
startText finishFun (Span textLeft _) _lbs = do
    modify' $ \s -> s
        { _parserState_textBuffer = ""
        , _parserState_textLeft = textLeft
        , _parserState_textFinish = Just finishFun
        , _parserState_lexState = textState
        }
    lexToken

startString :: Span -> Lazy.ByteString -> Parser Token
startString = startText $ do
    ParserState
        { _parserState_alexInput = AlexInput { _alexInput_sourcePos = textRight }
        , _parserState_textLeft = textLeft
        , _parserState_textBuffer = textBuffer
        } <- get
    begin 0
    pure $ At (Span textLeft textRight) $ Token.STRING textBuffer

startChar :: Span -> Lazy.ByteString -> Parser Token
startChar = startText $ do
    ParserState
        { _parserState_alexInput = AlexInput { _alexInput_sourcePos = sourcePos }
        , _parserState_textLeft = textLeft
        , _parserState_textBuffer = textBuffer
        } <- get
    begin 0
    c <- case Text.length textBuffer of
           1 -> pure $ Text.head textBuffer
           _ -> do
               withCurrentSpan (Span textLeft sourcePos) $
                   compilerExternalWarning "Character constant not of size 1"
               pure '\0'
    pure $ At (Span textLeft sourcePos) $ Token.CHAR c

finishText :: Span -> Lazy.ByteString -> Parser Token
finishText (Span _ textRight) _lbs = do
    gets _parserState_textFinish >>= \case
      Nothing -> compilerInternalError "Called 'finishText' without 'textFinish' state" "" CompilerBug
      Just m -> do
          modify' $ \s -> s { _parserState_textFinish = Nothing }
          m

addText :: Text -> Parser ()
addText t =
    modify' $ \s -> s
        { _parserState_textBuffer = _parserState_textBuffer s <> t
        }

startComment :: Parser () -> Span -> Parser ()
startComment th (Span commentLeft _) = do
    inComment <- gets _parserState_inComment
    if inComment
       then modify' $ \s -> s
           { _parserState_commentStack = th : _parserState_commentStack s
           }
       else modify' $ \s -> s
           { _parserState_commentStack = [th]
           , _parserState_commentLeft = commentLeft
           }

finishComment :: Parser ()
finishComment = do
    commentStack <- gets _parserState_commentStack
    case commentStack of
      [] -> compilerInternalError "Comment stack empty" "" CompilerBug
      (c:cs) -> do
          modify' $ \s -> s { _parserState_commentStack = cs }
          c

-- Alex Primitive
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput { _alexInput_input = input, _alexInput_sourcePos = sourcePos } =
    case Lazy.ByteString.uncons input of
      Nothing -> Nothing
      Just (w, rest) -> Just (w, AlexInput
          { _alexInput_input = rest
          , _alexInput_sourcePos =
              if w == c2w '\n'
                 then sourcePos
                     { _sourcePos_line = _sourcePos_line sourcePos + 1
                     , _sourcePos_col = 1
                     , _sourcePos_offset = _sourcePos_offset sourcePos + 1
                     }
                 else sourcePos
                     { _sourcePos_col = _sourcePos_col sourcePos + 1
                     , _sourcePos_offset = _sourcePos_offset sourcePos + 1
                     }
          })

}

