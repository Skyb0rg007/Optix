{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy       as Lazy.ByteString
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Text                  (Text)
import qualified Data.Text.IO               as Text.IO
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Foreign.LibFFI             as FFI
-- import           System.DlOpen
import           System.Environment
import           System.Exit
import           System.IO

-- import qualified Language.Optix.Core.SExp              as Core
-- import qualified Language.Optix.Core.Syntax            as Core
-- import qualified Language.Optix.Frontend.Lower         as Frontend
-- import qualified Language.Optix.Frontend.Parser.Lexer  as Frontend
-- import qualified Language.Optix.Frontend.Parser.Monad  as Frontend
-- import qualified Language.Optix.Frontend.Parser.Parser2 as Frontend
-- import qualified Language.Optix.Frontend.Parser.Token  as Frontend
-- import qualified Language.Optix.Frontend.AST        as Frontend
-- import qualified Language.Optix.Typecheck              as Typecheck

-- import           Language.Optix.Utils.Located
-- import           Language.Optix.Utils.Pretty
-- import qualified Language.Optix.Utils.PrecParse as PrecParse

-- fixityEnv :: HashMap Text PrecParse.Fixval
-- fixityEnv = HashMap.fromList
  -- [ ("**", PrecParse.Infix 17 16)

  -- , ("*", PrecParse.Infix 14 15)
  -- , ("mod", PrecParse.Infix 14 15)
  -- , ("div", PrecParse.Infix 14 15)

  -- , ("+", PrecParse.Infix 12 13)
  -- , ("-", PrecParse.Infix 12 13)

  -- , ("::", PrecParse.Infix 11 10)

  -- , ("=", PrecParse.Infix 8 8)
  -- , ("<", PrecParse.Infix 8 8)
  -- , (">", PrecParse.Infix 8 8)
  -- , ("<=", PrecParse.Infix 8 8)
  -- , (">=", PrecParse.Infix 8 8)

  -- , (":=", PrecParse.Infix 7 6)

  -- , ("before", PrecParse.Infix 0 1)
  -- ]

-- frontendStyle :: Frontend.Style -> AnsiStyle
-- frontendStyle = \case
    -- Frontend.StyleString -> colorDull Red
    -- Frontend.StyleInt -> colorDull Red
    -- Frontend.StyleKeyword -> color Green
    -- Frontend.StyleTyVar -> color Blue
    -- Frontend.StyleVar -> color Blue
    -- Frontend.StyleType -> colorDull Yellow
    -- Frontend.StyleLabel -> colorDull Blue
    -- Frontend.StyleBool -> colorDull Red

-- coreStyle :: Core.Style -> AnsiStyle
-- coreStyle = \case
    -- Core.StyleString -> colorDull Red
    -- Core.StyleInt -> colorDull Red
    -- Core.StyleKeyword -> color Green
    -- Core.StyleTyVar -> color Blue
    -- Core.StyleVar -> color Blue
    -- Core.StyleType -> colorDull Yellow
    -- Core.StyleLabel -> colorDull Blue
    -- Core.StyleBool -> colorDull Red

main :: IO ()
main = putStrLn "main"

{- 
main :: IO ()
main = do
    input <- Lazy.ByteString.getContents
    case Frontend.runParser input "<stdin>" Frontend.parseProgram of
      Left err -> putStrLn $ "Error: " ++ err
      Right prog -> do
          putStrLn "Input: \n------------------------------\n\n"
          -- print $ prettyPrec 0 prog
          putDoc (frontendStyle <$> prettyPrec 0 prog)
          putStrLn ""
          -- case Frontend.lower prog of
            -- Left err -> Text.IO.putStrLn $ "Error: " <> err
            -- Right prog' -> do
                -- putStrLn "\nCore: \n------------------------------\n\n"
                -- -- print $ prettyPrec 0 prog'
                -- putDoc (coreStyle <$> prettyPrec 0 prog')
                -- putStrLn ""
                -- case Typecheck.typecheck (Core._programBody prog') of
                  -- Left err -> Text.IO.putStrLn $ "Error: " <> err
                  -- Right _ -> putStrLn "Okay"
 -}
