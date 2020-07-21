
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Language.Optix.Core.SExp where

import qualified Bound
import           Bound.Name                   (Name (..))
import qualified Bound.Name
import qualified Bound.Scope                  as Bound
import           Control.Applicative          (many, some, (<|>))
import           Data.Foldable                (asum, foldlM)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Int                     (Int32)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap.Strict           as IntMap
import qualified Data.IntMap.Lazy             as Lazy.IntMap
import           Data.Maybe                   (fromMaybe, listToMaybe)
import           Data.SCargot
import           Data.SCargot.Repr.WellFormed
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Text.Parsec                  as Parsec
import           Text.Parsec.Text             (Parser)

import           Language.Optix.Core.Syntax
import           Language.Optix.Utils.Located
import           Language.Optix.Utils.Pretty (tshow)
import           Language.Optix.Utils.Fresh

data Atom
    = AtomWild -- '_'
    | AtomIdent !Text -- 'a
    | AtomTyVar !Text -- x
    | AtomSym !Text -- +
    | AtomKeyword !Text -- if
    | AtomInt !Int32 -- 12
    | AtomBool !Bool -- true
    -- | AtomUnit -- represented as empty list

showAtom :: Atom -> Text
showAtom = \case
    AtomWild -> "_"
    AtomIdent x -> x
    AtomTyVar x -> x
    AtomSym x -> x
    AtomKeyword x -> x
    AtomInt n -> tshow n
    AtomBool True -> "true"
    AtomBool False -> "false"

parseAtom :: Parser Atom
parseAtom =
        Parsec.try parseWild
    <|> Parsec.try parseKeyword
    <|> Parsec.try parseIdent
    <|> Parsec.try parseTyVar
    <|> Parsec.try parseSym
    <|> Parsec.try parseInt
    <|> parseBool
    where
        parseWild = AtomWild <$ Parsec.string "_"
        parseKeyword = fmap (AtomKeyword . Text.pack) $
            asum
            [ Parsec.string "if"
            , Parsec.string "fn"
            , Parsec.string "let"
            , Parsec.string "letrec"
            , Parsec.string "."
            , Parsec.string ":"
            , Parsec.string "record"
            ]
        parseIdent = fmap (AtomIdent . Text.pack) $
            (:) <$> Parsec.letter <*> many (Parsec.alphaNum <|> Parsec.oneOf "_'")
        parseTyVar = fmap (AtomTyVar . Text.pack) $
            (:) <$> Parsec.char '\'' <*>
            ((:) <$> Parsec.letter <*> many (Parsec.alphaNum <|> Parsec.oneOf "_'"))
        parseSym = fmap (AtomSym . Text.pack) $
            some (Parsec.oneOf "-!%&$#+/:<=>?@\\~`^|*")
        parseInt = fmap AtomInt $
                (parseNum 2  =<< Parsec.try (Parsec.string "0b" *> some (Parsec.oneOf "01")))
            <|> (parseNum 2  =<< Parsec.try (Parsec.string "0B" *> some (Parsec.oneOf "01")))
            <|> (parseNum 8  =<< Parsec.try (Parsec.string "0o" *> some Parsec.octDigit))
            <|> (parseNum 8  =<< Parsec.try (Parsec.string "0O" *> some Parsec.octDigit))
            <|> (parseNum 16 =<< Parsec.try (Parsec.string "0x" *> some Parsec.hexDigit))
            <|> (parseNum 16 =<< (Parsec.string "0X" *> some Parsec.hexDigit))
            <|> (parseNum 10 =<< Parsec.try (some Parsec.digit))
        parseBool =
                AtomBool True <$ Parsec.string "true"
            <|> AtomBool False <$ Parsec.string "false"

        parseNum :: Int32 -> String -> Parser Int32
        parseNum base text =
            let f :: Int32 -> Char -> Either String Int32
                f acc c =
                    let acc' =
                            if | c >= '0' && c <= '9' -> base * acc + fromIntegral (fromEnum c - fromEnum '0')
                               | c >= 'a' && c <= 'f' -> base * acc + fromIntegral (fromEnum c - fromEnum 'a' + 10)
                               | c >= 'A' && c <= 'F' -> base * acc + fromIntegral (fromEnum c - fromEnum 'A' + 10)
                               | otherwise -> acc
                     in if acc' < acc
                           then Left $ "Integer overflow for number: \"" ++ text ++ "\""
                           else Right acc'
             in case foldlM f 0 text of
                  Left err -> fail err
                  Right x -> pure x

decodeSExp :: Text -> Either String [WellFormedSExpr Atom]
decodeSExp = decode $ asWellFormed $ mkParser parseAtom

encodeSExp :: [WellFormedSExpr Atom] -> Text
encodeSExp = encode $
    -- setIndentAmount 4 $
    -- setIndentStrategy (const Swing) $
    setMaxWidth 80 $
    setFromCarrier (fmap showAtom . fromWellFormed) $
    basicPrint id


encodeExp :: Exp -> Text
encodeExp =
    encodeOne (setFromCarrier fromWellFormed $ unconstrainedPrint id)
    . flip runFresh mempty
    . toSExp
    where
        l = WFSList
        a = WFSAtom
        instFresh :: ExpScope () Text -> Fresh (Text, WellFormedSExpr Text)
        instFresh scope = do
            let name = listToMaybe $ Bound.Name.name <$> Bound.bindings scope
            case name of
              Nothing ->
                  ("_",) <$> toSExp (Bound.instantiate1 (ExpVar "_") scope)
              Just nm ->
                  (nm,) <$> avoid [nm] (toSExp $ Bound.instantiate1 (ExpVar nm) scope)
        toSExp :: Exp -> Fresh (WellFormedSExpr Text)
        toSExp = \case
            ExpVar x -> pure $ a x
            ExpApp e1 e2 -> l <$> traverse toSExp [e1, e2]
            ExpInt n -> pure $ a (tshow n)
            ExpBool True -> pure $ a "true"
            ExpBool False -> pure $ a "false"
            ExpUnit -> pure $ l []
            ExpIf e1 e2 e3 -> l . (a "if":) <$> traverse toSExp [e1, e2, e3]
            ExpFn scope -> do
                (name, body) <- instFresh scope
                pure $ l [a "fn", l [a name], body]
            ExpLet e1 e2 -> do
                e1' <- toSExp e1
                (x, e2') <- instFresh e2
                pure $ l [a "let", l [a x, e1'], e2']
            ExpLetRec binds body -> do
                let nameMap :: IntMap Text
                    nameMap =
                        foldr
                        (\(Name n b) -> IntMap.insert b n)
                        IntMap.empty
                        (concatMap Bound.bindings (body:binds))
                nameMap' <- traverse fresh nameMap
                let inst :: ExpScope Int Text -> Fresh (WellFormedSExpr Text)
                    inst = toSExp . Bound.instantiate (\(Name _ b) -> ExpVar $ IntMap.findWithDefault "_" b nameMap')
                binds' <- traverse (\(n, e) -> l <$> sequence [pure (a (IntMap.findWithDefault "_" n nameMap')), inst e]) (zip [0..] binds)
                body' <- inst body
                pure $ l [a "letrec", l binds', body']
            ExpRecord r -> do
                let f :: Text -> Exp -> Fresh [WellFormedSExpr Text] -> Fresh [WellFormedSExpr Text]
                    f k v flds = do
                        flds' <- flds
                        v' <- toSExp v
                        pure $ [a k, v'] ++ flds'
                flds <- HashMap.foldrWithKey f (pure []) r
                pure $ l $ a "record" : flds
            ExpGet e x -> do
                e' <- toSExp e
                pure $ l [a $ "." <> x, e']
            ExpConstraint e t -> do
                e' <- toSExp e
                t' <- pure $ a "TODO"
                pure $ l [a ":", e', t']
            ExpPrim x t -> do
                pure $ l [a "prim"]
            ExpLocated (At _ x) -> toSExp x

