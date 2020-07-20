{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.Optix.Utils.Located2
    ( MonadLocated (..)
    , LocatedT (..)
    , Located (..)
    , Span (..)
    , _spanLeft, _spanRight
    , SourcePos (..)
    , bogusSourcePos
    , spanSubstring
    ) where

import           Control.Applicative               (Alternative)
import           Control.DeepSeq                   (NFData)
import           Control.Monad.Base                (MonadBase (..))
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Except        as Except
import qualified Control.Monad.Trans.Identity      as Identity
import           Control.Monad.Trans.LFresh        (LFreshT)
import qualified Control.Monad.Trans.Maybe         as Maybe
import qualified Control.Monad.Trans.Reader        as Reader
import qualified Control.Monad.Trans.RWS.Lazy      as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict    as StrictRWS
import qualified Control.Monad.Trans.State.Lazy    as LazyState
import qualified Control.Monad.Trans.State.Strict  as StrictState
import           Control.Monad.Trans.Warn          (WarnT)
import qualified Control.Monad.Trans.Writer.Lazy   as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import           Data.Binary                       (Binary (..))
import           Data.Bytes.Serial                 (Serial, Serial1)
import           Data.Data                         (Data)
import           Data.Deriving                     (deriveEq1, deriveOrd1,
                                                    deriveRead1)
import           Data.Function                     (on)
import           Data.Functor.Classes
import           Data.Hashable                     (Hashable)
import           Data.Hashable.Lifted              (Hashable1)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Typeable                     (Typeable)
import           Data.Word                         (Word32)
import           GHC.Generics                      (Generic, Generic1)
import           Text.Printf                       (printf)

import           Language.Optix.Utils.Pretty2

-- * Locating the AST in the program text
-- Strategy comes from Rust, Elm, MLton

class Monad m => MonadLocated m where
    getCurrentSpan :: m Span
    withCurrentSpan :: Span -> m a -> m a

    default getCurrentSpan :: (MonadTrans t, MonadLocated n, m ~ t n) => m Span
    getCurrentSpan = lift getCurrentSpan
    default withCurrentSpan :: (MonadTransControl t, MonadLocated n, m ~ t n) => Span -> m a -> m a
    withCurrentSpan s m = liftWith (\run -> withCurrentSpan s (run m)) >>= restoreT . pure

newtype LocatedT m a = LocatedT { unLocatedT :: ReaderT Span m a }
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadFix
        , MonadIO
        , MonadPlus
        , MonadTrans
        , MonadTransControl
        )

instance Monad m => MonadLocated (LocatedT m) where
    getCurrentSpan = LocatedT ask
    withCurrentSpan s = LocatedT . local (const s) . unLocatedT

instance MonadBase IO m => MonadBase IO (LocatedT m) where
    liftBase = lift . liftBase

instance MonadBaseControl IO m => MonadBaseControl IO (LocatedT m) where
    type StM (LocatedT m) a = ComposeSt LocatedT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadLocated m              => MonadLocated (Except.ExceptT e' m)
instance MonadLocated m              => MonadLocated (Identity.IdentityT m)
instance MonadLocated m              => MonadLocated (Maybe.MaybeT m)
instance MonadLocated m              => MonadLocated (Reader.ReaderT r m)
instance (Monoid w', MonadLocated m) => MonadLocated (LazyRWS.RWST r w' s m)
instance (Monoid w', MonadLocated m) => MonadLocated (StrictRWS.RWST r w' s m)
instance MonadLocated m              => MonadLocated (LazyState.StateT s m)
instance MonadLocated m              => MonadLocated (StrictState.StateT s m)
instance (Monoid w', MonadLocated m) => MonadLocated (LazyWriter.WriterT w' m)
instance (Monoid w', MonadLocated m) => MonadLocated (StrictWriter.WriterT w' m)

instance MonadLocated m => MonadLocated (LFreshT m)
instance MonadLocated m => MonadLocated (WarnT w e m)

-- | Attach a location onto an AST node
data Located a = At !Span !a
    deriving (Eq, Ord, Generic, Generic1, Data, Typeable, NFData, Binary, Functor, Foldable, Traversable, Serial, Serial1, Hashable, Hashable1)

instance Show1 Located where
    liftShowsPrec sp sl d (At _ x) = sp d x

instance Show a => Show (Located a) where
    showsPrec d (At _ x) = showsPrec d x

instance Applicative Located where
    pure = At mempty
    At s1 a1 <*> At s2 a2 = At (s1 <> s2) (a1 a2)

instance Pretty a => Pretty (Located a)  where
    type Style (Located a) = Style a
    prettyPrec d (At _ a) = prettyPrec d a

-- | A source-code location
data SourcePos = SourcePos
    { _sourcePosFile   :: !FilePath
    , _sourcePosOffset :: !Int
    , _sourcePosLine   :: !Int
    , _sourcePosCol    :: !Int
    }
    deriving (Show, Eq, Ord, Read, Generic, Data, Typeable, NFData, Binary, Serial, Hashable)

bogusSourcePos :: SourcePos
bogusSourcePos = SourcePos "<bogus>" (-1) (-1) (-1)

instance Pretty SourcePos where
    pretty (SourcePos file _ line col) = pretty file <+> pretty line <> "." <> pretty col

-- | A source code span
data Span
    = Span !SourcePos !SourcePos
    | BogusSpan
    deriving (Show, Eq, Ord, Read, Generic, Data, Typeable, NFData, Binary, Serial, Hashable)

_spanLeft, _spanRight :: Span -> Maybe SourcePos
_spanLeft (Span l _) = Just l
_spanLeft BogusSpan  = Nothing
_spanRight (Span _ r) = Just r
_spanRight BogusSpan  = Nothing

-- TODO: follow GCC error styling conventions
instance Pretty Span where
    pretty BogusSpan = pretty bogusSourcePos
    pretty (Span left right)
      | left == bogusSourcePos || right == bogusSourcePos || on (/=) _sourcePosFile left right = pretty left
      | otherwise = pretty left <> "-" <> pretty right

instance Semigroup Span where
    BogusSpan <> s = s
    s <> BogusSpan = s
    Span left _ <> Span _ right = Span left right

instance Monoid Span where
    mempty = BogusSpan
    mappend = (<>)

spanSubstring :: Span -> Text -> Text
spanSubstring (Span (SourcePos _ start _ _) (SourcePos _ end _ _)) str =
    Text.drop (fromIntegral start) $ Text.take (fromIntegral end) str

deriveEq1 ''Located
deriveOrd1 ''Located

