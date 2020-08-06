
module Language.Optix.Frontend.Parser.Monad where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Warn
import qualified Data.ByteString.Lazy        as Lazy (ByteString)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Maybe
import           Language.Optix.Util.Error
import           Language.Optix.Util.Located
import           Language.Optix.Util.Pretty
import           Language.Optix.Frontend.Parser.Token (Token)
import           Data.Sequence (Seq)

data AlexInput = AlexInput
    { _alexInput_sourcePos :: !SourcePos
    , _alexInput_input     :: !Lazy.ByteString
    }
    deriving (Show)

data ParserState = ParserState
    -- Comments
    { _parserState_commentLeft   :: !SourcePos
    , _parserState_commentStack  :: ![Parser ()]
    -- Text constants
    , _parserState_textBuffer    :: !Text
    , _parserState_textLeft      :: !SourcePos
    , _parserState_textFinish    :: !(Maybe (Parser Token))
    -- Alex State
    , _parserState_alexInput     :: !AlexInput
    , _parserState_lexState      :: !Int
    }

-- This state is kept in the MLton source, but is combined with textFinish here
{- HLINT ignore "Use camelCase" -}
_parserState_inText :: ParserState -> Bool
_parserState_inText = isJust . _parserState_textFinish

{- HLINT ignore "Use camelCase" -}
_parserState_inComment :: ParserState -> Bool
_parserState_inComment = not . null . _parserState_commentStack

initialState :: AlexInput -> ParserState
initialState alexInput = ParserState
    { _parserState_commentLeft   = bogusSourcePos
    , _parserState_commentStack  = []
    , _parserState_textBuffer    = ""
    , _parserState_textLeft      = bogusSourcePos
    , _parserState_textFinish    = Nothing
    , _parserState_alexInput     = alexInput
    , _parserState_lexState      = 0
    }

runParser :: Parser a -> Span -> AlexInput -> (Either CompilerError a, Seq CompilerWarning)
runParser (P m) span alexInput =
    runWarn $
    flip evalStateT (initialState alexInput) $
    runLocatedT m span

newtype Parser a = P (LocatedT (StateT ParserState (Warn CompilerWarning CompilerError)) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadWarn CompilerWarning CompilerError
        , MonadLocated
        , MonadState ParserState
        )

