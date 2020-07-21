
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Optix.Frontend.Parser.Monad
    ( Parser
    , ParserState (..)
    , AlexInput (..)
    , runParser
    ) where

import           Control.Monad.Except       (Except, MonadError, runExcept)
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import qualified Data.ByteString.Lazy       as Lazy (ByteString)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Language.Optix.Utils.Located (SourcePos (SourcePos))

data AlexInput = AlexInput
    { _alexInputSourcePos :: !SourcePos 
    , _alexInputInput     :: !Lazy.ByteString
    }
    deriving (Show)

data ParserState = ParserState
    { _parserStateAlexInput    :: !AlexInput
    , _parserStateCommentStack :: ![SourcePos]
    , _parserStateTextBuffer   :: !Text
    , _parserStateInText       :: !Bool
    , _parserStateInChar       :: !Bool
    , _parserStateTextLeft     :: !SourcePos
    , _parserStateFileName     :: !FilePath
    , _parserStateLexState     :: !Int
    }
    deriving (Show)

bogusSP :: SourcePos
bogusSP = SourcePos 0 1 1

runParser :: Lazy.ByteString -> FilePath -> Parser a -> Either String a
runParser lbs file (P p) =
    runExcept $ p `evalStateT`
    ParserState
        { _parserStateAlexInput = AlexInput bogusSP lbs
        , _parserStateCommentStack = []
        , _parserStateTextBuffer = Text.empty
        , _parserStateInText = False
        , _parserStateInChar = False
        , _parserStateTextLeft = bogusSP
        , _parserStateFileName = file
        , _parserStateLexState = 0
        }

-- TODO: Except String -> Except Text
newtype Parser a = P (StateT ParserState (Except String) a)
    deriving newtype
            ( Functor
            , Applicative
            , Monad
            , MonadError String
            , MonadState ParserState
            )

