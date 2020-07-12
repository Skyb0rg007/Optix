{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Optix.Utils.Located
    ( Located (..)
    , Span (..)
    , SourcePos (..)
    , spanSubstring
    ) where

import           Control.DeepSeq      (NFData)
import           Control.Exception    (assert)
import           Data.Binary          (Binary (..))
import           Data.Bytes.Serial    (Serial, Serial1)
import           Data.Data            (Data)
import           Data.Deriving        (deriveEq1, deriveOrd1, deriveRead1)
import           Data.Functor.Apply   (Apply ((<.>)))
import           Data.Functor.Classes
import           Data.Hashable        (Hashable)
import           Data.Hashable.Lifted (Hashable1)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Typeable        (Typeable)
import           Data.Word            (Word32)
import           GHC.Generics         (Generic, Generic1)
import           Text.Printf          (printf)

import           Language.Optix.Utils.Pretty

-- * Locating the AST in the program text
-- Strategy comes from the Rust AST and Elm

-- | Attach a location onto an AST node
data Located a = At !Span !a
    deriving (Eq, Ord, Generic, Generic1, Data, Typeable, NFData, Binary, Functor, Foldable, Traversable, Serial, Serial1, Hashable, Hashable1)

instance Show1 Located where
    liftShowsPrec sp sl d (At _ x) = sp d x

instance Show a => Show (Located a) where
    showsPrec d (At _ x) = showsPrec d x

instance Apply Located where
    At s1 a1 <.> At s2 a2 = At (s1 <> s2) (a1 a2)

instance PrettyPrec a ann => PrettyPrec (Located a) ann where
    prettyPrec d (At _ a) = prettyPrec d a

-- | A source-code location
data SourcePos = SourcePos
    { _sourcePosOffset :: !Word32
    , _sourcePosLine   :: !Word32
    , _sourcePosCol    :: !Word32
    }
    deriving (Eq, Ord, Read, Generic, Data, Typeable, NFData, Binary, Serial, Hashable)

instance Show SourcePos where
    showsPrec d (SourcePos _ line col) = showParen (d > 0) $
        showString $ printf "%u.%u" line col

minSourcePos, maxSourcePos :: SourcePos -> SourcePos -> SourcePos
minSourcePos s1 s2 =
    if _sourcePosOffset s1 < _sourcePosOffset s2
       then s1
       else s2
maxSourcePos s1 s2 =
    if _sourcePosOffset s1 < _sourcePosOffset s2
       then s2
       else s1

-- | A source code span
data Span = Span
    { _spanFileName :: !FilePath
    , _spanStart    :: !SourcePos
    , _spanEnd      :: !SourcePos
    }
    deriving (Eq, Ord, Read, Generic, Data, Typeable, NFData, Binary, Serial, Hashable)

instance Show Span where
    showsPrec d (Span file (SourcePos _ startLine startCol) (SourcePos _ endLine endCol)) = showParen (d > 0) $
        if startLine == endLine 
           then showString $ printf "%s:%u.%u-%u" file startLine startCol endCol
           else showString $ printf "%s:%u.%u-%u.%u" file startLine startCol endLine endCol
instance Semigroup Span where
    s1 <> s2 = assert (_spanFileName s1 == _spanFileName s2) $
        Span { _spanFileName = _spanFileName s1
             , _spanStart = minSourcePos (_spanStart s1) (_spanStart s2)
             , _spanEnd = maxSourcePos (_spanEnd s1) (_spanEnd s2)
             }

spanSubstring :: Span -> Text -> Text
spanSubstring (Span _ (SourcePos start _ _) (SourcePos end _ _)) str =
    Text.drop (fromIntegral start) $ Text.take (fromIntegral end) str

deriveEq1 ''Located
deriveOrd1 ''Located
deriveRead1 ''Located

