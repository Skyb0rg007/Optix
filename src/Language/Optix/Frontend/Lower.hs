{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Language.Optix.Frontend.Lower where

import qualified Bound
import qualified Bound.Name
import           Control.DeepSeq                       (NFData)
import           Control.Exception                     (assert)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Binary                           (Binary)
import           Data.Data                             (Data)
import           Data.Foldable                         (foldlM)
import           Data.Functor                          (($>))
import           Data.Hashable                         (Hashable)
import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import qualified Data.HashSet as HashSet
import           Data.Int                              (Int32)
import           Data.List                             (groupBy, elemIndex)
import           Data.List.NonEmpty                    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                    as NonEmpty
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           (Endo (Endo, appEndo))
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (Pretty (pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import           Data.Typeable                         (Typeable)
import qualified Foreign.LibFFI                        as LibFFI
import           GHC.Generics                          (Generic)
import           GHC.Stack                             (HasCallStack)
import           Text.Megaparsec.Pos                   (SourcePos (..), unPos)

import qualified Language.Optix.Core.Syntax     as Core
import qualified Language.Optix.Frontend.Syntax as Frontend
import           Language.Optix.Utils.Located
import           Language.Optix.Utils.Misc      (foldMapM)
import qualified Language.Optix.Utils.PrecParse as PrecParse
import           Language.Optix.Utils.Pretty

type FixityEnv = HashMap Text PrecParse.Fixval

lower :: MonadError Text m => Frontend.Program -> m Core.Program
lower (Frontend.Program decls) = 
    flip evalStateT HashMap.empty $ do
        expEndo <- lowerDecls decls
        pure $ Core.Program $ appEndo expEndo Core.ExpUnit

lowerDecls
    :: (MonadError Text m, MonadState FixityEnv m)
    => [Frontend.Decl]
    -> m (Endo Core.Exp)
lowerDecls = foldMapM lowerDecl

lowerDecl
    :: forall m. (MonadError Text m, MonadState FixityEnv m)
    => Frontend.Decl
    -> m (Endo Core.Exp)
lowerDecl (At _ x) = lowerDecl_ x

lowerDecl_
    :: forall m. (MonadError Text m, MonadState FixityEnv m)
    => Frontend.Decl_
    -> m (Endo Core.Exp)
lowerDecl_ = \case
    Frontend.DeclFixity fixityDecl -> lowerFixity fixityDecl
    Frontend.DeclVal valDecls -> foldMapM lowerVal valDecls
    Frontend.DeclFun funDecls -> lowerFuns funDecls
    Frontend.DeclDo e -> lowerDo e
    where
        lowerFixity :: Frontend.FixityDecl -> m (Endo Core.Exp)
        lowerFixity (Frontend.FixityDecl fixity ids) =
            let fixval :: PrecParse.Fixval
                fixval =
                    case fixity of
                      Frontend.Infix n  -> PrecParse.Infix (n+n)   (n+n)
                      Frontend.Infixl n -> PrecParse.Infix (n+n)   (n+n+1)
                      Frontend.Infixr n -> PrecParse.Infix (n+n+1) (n+n)
                      Frontend.Nonfix   -> PrecParse.Nonfix
                modification :: HashMap Text PrecParse.Fixval
                modification = foldr (`HashMap.insert` fixval) HashMap.empty ids
             in modify' (HashMap.union modification) $> mempty

        lowerVal :: Frontend.ValDecl -> m (Endo Core.Exp)
        lowerVal (Frontend.ValDecl (At _ Frontend.PatWild) body) = do
            fixityEnv <- get
            e1 <- lowerExp body `runReaderT` fixityEnv
            pure $ Endo $ \e2 -> Core.ExpLet e1 (Bound.Name.abstractName (const Nothing) e2)
        lowerVal (Frontend.ValDecl (At _ (Frontend.PatVar x _ Nothing)) body) = do
            fixityEnv <- get
            e1 <- lowerExp body `runReaderT` fixityEnv
            pure $ Endo $ \e2 -> Core.ExpLet e1 (Bound.Name.abstract1Name x e2)
        lowerVal _ = throwError "Type annotations on 'val' is not supported"

        lowerDo :: Frontend.Exp -> m (Endo Core.Exp)
        lowerDo e = do
            fixityEnv <- get
            e' <- lowerExp e `runReaderT` fixityEnv
            pure $ Endo $ \e2 -> Core.ExpLet e' (Bound.Name.abstractName (const Nothing) e2)

        lowerFuns :: NonEmpty Frontend.FunDecl -> m (Endo Core.Exp)
        lowerFuns funDecls = do
            funDecls' <- traverse funDeclToExp (NonEmpty.toList funDecls)
            let names :: [Text]
                bodies :: [Core.Exp]
                (names, bodies) = unzip funDecls'
                abstract :: Core.Exp -> Core.ExpScope Int Text
                abstract = Bound.Name.abstractName $ \name -> name `elemIndex` names
                binds :: [Core.ExpScope Int Text]
                binds = map abstract bodies
            pure $ Endo $ \body -> Core.ExpLetRec binds $ abstract body
        funDeclToExp :: Frontend.FunDecl -> m (Text, Core.Exp)
        funDeclToExp (Frontend.FunDecl pats Nothing body) = do
            fixityEnv <- get
            (At _ nm, pats') <- parsePrecClause (NonEmpty.toList pats) `runReaderT` fixityEnv
            body' <- lowerExp body `runReaderT` fixityEnv
            e <-
                let f :: Core.Exp -> Frontend.Pat -> m Core.Exp
                    f e (At _ Frontend.PatWild) =
                        pure $ Core.ExpFn $ Bound.Name.abstractName (const Nothing) e
                    f e (At _ (Frontend.PatVar x _ Nothing)) =
                        pure $ Core.ExpFn $ Bound.Name.abstract1Name x e
                    f _ _ = throwError "Type annotation not supported"
                 in foldlM f body' pats'
            pure (nm, e)
        funDeclToExp _ = throwError "Return type not supported"

lowerExp
    :: (MonadReader FixityEnv m, MonadError Text m)
    => Frontend.Exp
    -> m Core.Exp
lowerExp (At _ x) = lowerExp_ x

lowerExp_
    :: forall m. (MonadReader FixityEnv m, MonadError Text m)
    => Frontend.Exp_
    -> m Core.Exp
lowerExp_ = \case
    Frontend.ExpFlatApp es -> lowerExp =<< parsePrecExp (NonEmpty.toList es)
    Frontend.ExpApp e1 e2 -> Core.ExpApp <$> lowerExp e1 <*> lowerExp e2
    Frontend.ExpVar x _ -> pure $ Core.ExpVar x
    Frontend.ExpInt n -> pure $ Core.ExpInt n
    Frontend.ExpBool b -> pure $ Core.ExpBool b
    Frontend.ExpUnit -> pure Core.ExpUnit
    Frontend.ExpIf e1 e2 e3 -> Core.ExpIf <$> lowerExp e1 <*> lowerExp e2 <*> lowerExp e3
    Frontend.ExpFn (At _ Frontend.PatWild) body ->
        Core.ExpFn . Bound.Name.abstractName (const Nothing) <$> lowerExp body
    Frontend.ExpFn (At _ (Frontend.PatVar x _ Nothing)) body ->
        Core.ExpFn . Bound.Name.abstract1Name x <$> lowerExp body
    Frontend.ExpFn (At _ (Frontend.PatVar x _ Just {})) body ->
        throwError "Type annotations on 'fn' is not implemented"
    Frontend.ExpLet decls body -> do
        fixityEnv <- ask
        endo <- lowerDecls decls `evalStateT` fixityEnv
        appEndo endo <$> lowerExp body
    Frontend.ExpRecord r ->
        let f :: HashMap Text Core.Exp -> (Text, Frontend.Exp) -> m (HashMap Text Core.Exp)
            f m (k, v)
              | k `HashMap.member` m = throwError $ "Duplicate record field: \"" <> k <> "\""
              | otherwise = HashMap.insert k <$> lowerExp v <*> pure m
         in Core.ExpRecord <$> foldlM f HashMap.empty r
    Frontend.ExpGet e x -> Core.ExpGet <$> lowerExp e <*> pure x
    Frontend.ExpConstraint e t -> throwError "Contraints not yet implemented"
    Frontend.ExpPrim name ty -> throwError "Prim not yet implemented"

-- * Precedence Parsing

parsePrecClause
    :: forall m. (MonadReader FixityEnv m, MonadError Text m)
    => [Frontend.Pat]
    -> m (Located Text, [Frontend.Pat])
parsePrecClause pats = do
    fixityEnv <- ask
    let fixity :: Frontend.Pat -> PrecParse.Fixval
        fixity = \case
            At _ (Frontend.PatVar x False _)
              | Just f <- HashMap.lookup x fixityEnv -> f
            _ -> PrecParse.Nonfix
        process :: [Frontend.Pat] -> m (Located Text, [Frontend.Pat])
        process [] = throwError "Null pats"
        process (p@(At s (Frontend.PatVar x _ Nothing)) : args)
          | all ((== PrecParse.Nonfix) . fixity) args
          , fixity p == PrecParse.Nonfix = pure (At s x, args)
        process (a : p@(At s (Frontend.PatVar x _ Nothing)) : args)
          | fixity a == PrecParse.Nonfix
          , all ((== PrecParse.Nonfix) . fixity) args
          , PrecParse.Infix _ _ <- fixity p = pure (At s x, a : args)
        process _ = throwError "Ill-placed infix in a fun clause" -- TODO: improve
     in process pats

parsePrecExp
    :: forall m. (MonadReader FixityEnv m, MonadError Text m)
    => [Frontend.Exp]
    -> m Frontend.Exp
parsePrecExp es =
    let precParse :: PrecParse.PrecParse m Frontend.Exp
        precParse = PrecParse.PrecParse
            { PrecParse._precParseApply =
                \f@(At s1 _) a@(At s2 _) ->
                    pure $ At (s1 <> s2) $ Frontend.ExpApp f a
            , PrecParse._precParseApplyInfix =
                \f@(At s1 _) a@(At s2 _) b@(At s3 _) ->
                    pure $ At (s1 <> s3) $ Frontend.ExpApp (At (s1 <> s2) $ Frontend.ExpApp f a) b
            , PrecParse._precParseFixval =
                \(At _ e) -> do
                    fixityEnv <- ask
                    case e of
                      Frontend.ExpVar x False 
                        | Just f <- HashMap.lookup x fixityEnv -> pure f
                      _ -> pure PrecParse.Nonfix
            , PrecParse._precParseError =
                \err -> throwError $ "Error: " <> tshow err
            }
     in PrecParse.parse precParse es


