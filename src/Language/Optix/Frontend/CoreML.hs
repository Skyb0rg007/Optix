{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Language.Optix.Frontend.CoreML
    ( module Language.Optix.Frontend.CoreML
    , Const (..)
    , Ty (..)
    ) where

import           Control.DeepSeq           (NFData)
import           Data.Binary               (Binary)
import           Data.Data                 (Data)
import           Data.Functor              ((<&>))
import           Data.Hashable             (Hashable)
import           Data.Int                  (Int32)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

import qualified Language.Optix.Frontend.AST  as AST
import           Language.Optix.Utils.Located
import           Language.Optix.Utils.Pretty

-- Same as Language.Optix.Frontend.AST
type Const = AST.Const
type Ty = AST.Ty

-- Pattern
data Pat = Pat PatNode Ty
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)
data PatNode
    = PatCon Text [Pat]
    | PatConst Const
    | PatAs Text Pat
    | PatVar Text
    | PatWild
    deriving (Show, Eq, Ord, Generic, Binary, Hashable, NFData, Data, Typeable)

-- Expression
data Exp
    = ExpApp Exp Exp
    -- | ExpCase
    | ExpConst Const
    | ExpLambda Text Ty Exp
    | ExpLet [Dec] Exp
    | ExpSequence [Exp]
    | ExpVar Text
    -- | ExpCon 

data Dec
    = Dec


