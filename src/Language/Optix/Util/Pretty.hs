
module Language.Optix.Util.Pretty
    (
    -- * Class
      Pretty (..)
    -- * Combinators
    , module X
    , parenIf
    -- * Terminal output
    , AnsiStyle (..)
    , StyleAnsi (..)
    , putDocTerminal
    , putDocLnTerminal
    , hPutDocTerminal
    , hPutDocLnTerminal
    , printTerminal
    , printLnTerminal
    -- * Text output
    , prettyText
    , prettyLText
    -- * Rendering
    , renderText
    , renderLText
    , renderTerminal
    , renderShowS
    , renderString
    ) where

import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import qualified Data.Text.Lazy                            as Text.Lazy
import qualified Data.Text.Lazy                            as Lazy (Text)
import           Data.Text.Prettyprint.Doc                 as X hiding (Pretty (..))
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String   as Pretty.String
import           Data.Text.Prettyprint.Doc.Render.Terminal as X (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import           Data.Void                                 (Void, absurd)
import           System.IO                                 (Handle)

-- * Helpers

-- | Wraps the document in parentheses if the first argument is true
parenIf :: Bool -> Doc ann -> Doc ann
parenIf True = Pretty.parens
parenIf False = id
{-# INLINE parenIf #-}

-- | Class for pretty-printing documents
class Pretty a where
    type Style a
    -- | Pretty-print, no parentheses
    pretty :: a -> Doc (Style a)
    -- | Pretty-print given a precedence level
    prettyPrec :: Int -> a -> Doc (Style a)
    -- | Pretty-print a list, used to pretty-print Strings
    prettyList :: [a] -> Doc (Style a)
    {-# MINIMAL pretty | prettyPrec #-}

    type Style a = Void
    pretty = prettyPrec 0
    {-# INLINE pretty #-}
    prettyPrec _ = pretty
    {-# INLINE prettyPrec #-}
    prettyList = align . list . map pretty
    {-# INLINE prettyList #-}

-- | Class for converting a document's style into AnsiStyle
class StyleAnsi a where
    ansiStyle :: a -> AnsiStyle

-- | Print a document to the terminal
putDocTerminal :: (StyleAnsi a) => Doc a -> IO ()
putDocTerminal = Pretty.Terminal.putDoc . fmap ansiStyle
{-# INLINE putDocTerminal #-}

-- | Print a document to the terminal with a trailing newline
putDocLnTerminal :: (StyleAnsi a) => Doc a -> IO ()
putDocLnTerminal d = putDocTerminal d >> putStrLn ""
{-# INLINE putDocLnTerminal #-}

-- | Print a document to the terminal
hPutDocTerminal :: (StyleAnsi a) => Handle -> Doc a -> IO ()
hPutDocTerminal h = Pretty.Terminal.hPutDoc h . fmap ansiStyle
{-# INLINE hPutDocTerminal #-}

-- | Print a document to the terminal with a trailing newline
hPutDocLnTerminal :: (StyleAnsi a) => Handle -> Doc a -> IO ()
hPutDocLnTerminal h d = hPutDocTerminal h d >> putStrLn ""
{-# INLINE hPutDocLnTerminal #-}

-- | Print to the terminal
printTerminal :: (Pretty a, StyleAnsi (Style a)) => a -> IO ()
printTerminal = putDocTerminal . pretty
{-# INLINE printTerminal #-}

-- | Print to the terminal with a trailing newline
printLnTerminal :: (Pretty a, StyleAnsi (Style a)) => a -> IO ()
printLnTerminal = putDocLnTerminal . pretty
{-# INLINE printLnTerminal #-}

-- * Renderers

-- | Render to strict text
renderText :: SimpleDocStream a -> Text
renderText = Pretty.Text.renderStrict
{-# INLINE renderText #-}

-- | Render to lazy text
renderLText :: SimpleDocStream a -> Lazy.Text
renderLText = Pretty.Text.renderLazy
{-# INLINE renderLText #-}

-- | Render to ShowS
renderShowS :: SimpleDocStream a -> ShowS
renderShowS = Pretty.String.renderShowS
{-# INLINE renderShowS #-}

-- | Render to String
renderString :: SimpleDocStream a -> String
renderString = Pretty.String.renderString
{-# INLINE renderString #-}

-- | Render to terminal
renderTerminal :: Handle -> SimpleDocStream AnsiStyle -> IO ()
renderTerminal = Pretty.Terminal.renderIO
{-# INLINE renderTerminal #-}

-- Render a 'Pretty a' directly to 'Text'
prettyText :: Pretty a => a -> Text
prettyText = Pretty.Text.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty
{-# INLINE prettyText #-}

-- Render a 'Pretty a' directly to 'Lazy.Text'
prettyLText :: Pretty a => a -> Lazy.Text
prettyLText = Pretty.Text.renderLazy . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty
{-# INLINE prettyLText #-}

-- * Instances

instance StyleAnsi Void where
    ansiStyle = absurd
instance StyleAnsi AnsiStyle where
    ansiStyle = id

instance Pretty Char where
    pretty = Pretty.pretty
    prettyList = pretty . Text.pack
instance Pretty Double where
    pretty = Pretty.pretty
instance Pretty Float where
    pretty = Pretty.pretty
instance Pretty Int where
    pretty = Pretty.pretty
instance Pretty Integer where
    pretty = Pretty.pretty
instance Pretty () where
    pretty = Pretty.pretty
instance Pretty Void where
    pretty = Pretty.pretty
instance Pretty Text where
    pretty = Pretty.pretty
instance Pretty Lazy.Text where
    pretty = Pretty.pretty
instance Pretty a => Pretty [a] where
    type Style [a] = Style a
    pretty = prettyList
instance (Pretty a, Pretty b, Style a ~ Style b) => Pretty (a, b) where
    type Style (a, b) = Style a
    pretty (a, b) = tupled [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c, Style a ~ Style b, Style b ~ Style c) => Pretty (a, b, c) where
    type Style (a, b, c) = Style a
    pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]

