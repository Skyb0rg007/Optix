
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Optix.Utils.Error
    ( MonadCompilerError
    , CompilerWarning (..)
    , CompilerError (..)
    , ErrorClass (..)
    , InternalError (..)
    , compilerExternalError
    , compilerExternalWarning
    , compilerInternalError
    , internalError
    ) where

import           Control.Exception  (Exception, throw)
import           Control.Monad.Warn
import           Data.Text          (Text)
import           Data.Void          (vacuous)
import           GHC.Stack          (HasCallStack)

import           Language.Optix.Utils.Located2
import           Language.Optix.Utils.Pretty2

type MonadCompilerError m = (MonadWarn CompilerWarning CompilerError m)

compilerExternalError
    :: (MonadCompilerError m, MonadLocated m)
    => Doc AnsiStyle
    -> m a
compilerExternalError d = do
    s <- getCurrentSpan
    err $ CompilerErrorExternal s d

compilerInternalError
    :: MonadCompilerError m
    => Text
    -> Text
    -> ErrorClass
    -> m a
compilerInternalError short long typ = err $ CompilerErrorInternal short long typ

compilerInternalWarning
    :: MonadCompilerError m
    => Text
    -> Text
    -> ErrorClass
    -> m ()
compilerInternalWarning short long typ = warn $ CompilerWarningInternal short long typ

compilerExternalWarning
    :: (MonadCompilerError m, MonadLocated m)
    => Doc AnsiStyle
    -> m ()
compilerExternalWarning d = do
    s <- getCurrentSpan
    warn $ CompilerWarningExternal s d

data ErrorClass
    = CompilerBug
    | CompilerLimitation
    deriving (Show, Eq, Ord, Read)

data CompilerError
    = CompilerErrorInternal !Text !Text !ErrorClass
    | CompilerErrorExternal !Span !(Doc AnsiStyle)

data CompilerWarning
    = CompilerWarningInternal !Text !Text !ErrorClass
    | CompilerWarningExternal !Span !(Doc AnsiStyle)

instance Pretty CompilerError where
    type Style CompilerError = AnsiStyle
    pretty (CompilerErrorInternal s _ _) = vacuous (pretty s)
    pretty (CompilerErrorExternal s d) = vacuous (pretty s) <> ":" <+> d

instance Pretty CompilerWarning where
    type Style CompilerWarning = AnsiStyle
    pretty (CompilerWarningInternal s _ _) = vacuous (pretty s)
    pretty (CompilerWarningExternal s d) = vacuous (pretty s) <> ":" <+> d

data InternalError = InternalError !Text !ErrorClass
    deriving (Show)
instance Exception InternalError

internalError :: HasCallStack => Text -> ErrorClass -> a
internalError desc typ = throw $ InternalError desc typ

