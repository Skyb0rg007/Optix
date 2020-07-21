{-# LANGUAGE FunctionalDependencies #-}

module Language.Optix.Utils.Pretty
    ( prettyText
    , prettyLText
    , tshow
    , ltshow
    , parenIf
    , PrettyPrec (prettyPrec)
    ) where

import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Lazy                        as Text.Lazy
import qualified Data.Text.Lazy                        as Lazy (Text)
import           Data.Text.Prettyprint.Doc             (Pretty (pretty), Doc)
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text

-- Render a 'Pretty a' directly to 'Text'
prettyText :: Pretty a => a -> Text
prettyText = Pretty.Text.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty
{-# INLINE prettyText #-}

-- Render a 'Pretty a' directly to 'Lazy.Text'
prettyLText :: Pretty a => a -> Lazy.Text
prettyLText = Pretty.Text.renderLazy . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty
{-# INLINE prettyLText #-}

-- Convert a 'Show a' directly to 'Text'
tshow :: Show a => a -> Text
tshow = Text.pack . show
{-# INLINE tshow #-}

-- Convert a 'Show a' directly to 'Text'
ltshow :: Show a => a -> Lazy.Text
ltshow = Text.Lazy.pack . show
{-# INLINE ltshow #-}

-- Useful helper
parenIf :: Bool -> Doc ann -> Doc ann
parenIf True = Pretty.parens
parenIf False = id

class PrettyPrec a ann | a -> ann where
    prettyPrec :: Int -> a -> Doc ann

