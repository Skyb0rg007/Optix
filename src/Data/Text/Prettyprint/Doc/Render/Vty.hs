-- Copied from prettyprinter-vty, updated to be simpler
-- Also the hackage library isn't available for recent stackage lts
{-# LANGUAGE LambdaCase #-}

module Data.Text.Prettyprint.Doc.Render.Vty
    ( render
    , renderSDS
    ) where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Internal (textSpaces)
import           Graphics.Vty.Attributes            (Attr)
import           Graphics.Vty.Image                 (Image, char, horizJoin, text', vertJoin)

render :: Doc Attr -> Image
render = renderSDS . layoutPretty defaultLayoutOptions

renderSDS :: SimpleDocStream Attr -> Image
renderSDS = go mempty [] mempty
    where
        go :: Attr -> [Attr] -> Image -> SimpleDocStream Attr -> Image
        go attr stack line = \case
            SFail -> error "Data.Text.Prettyprint.Doc.Render.Vty.renderSDS: SFail"
            SEmpty -> line
            SChar c x -> go attr stack (line `horizJoin` char attr c) x
            SText _ t x -> go attr stack (line `horizJoin` text' attr t) x
            SLine n x -> line `vertJoin` go attr stack (text' mempty (textSpaces n)) x
            SAnnPop x ->
                case stack of
                  [] -> error "Data.Text.Prettyprint.Doc.Render.Vty.renderSDS: Unmatched annotation"
                  ann:anns -> go ann anns line x


