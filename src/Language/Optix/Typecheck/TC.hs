
module Language.Optix.Typecheck.TC where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Warn
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Scientific      (Scientific)
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Void            (vacuous)
import           Numeric.Natural      (Natural)
import           Data.Function ((&))

import           Language.Optix.Frontend.Syntax
import           Language.Optix.Util.Error
import           Language.Optix.Util.Located
import qualified Language.Optix.Util.PrecParse  as PrecParse
import           Language.Optix.Util.Pretty

infix 0 <:

pattern ExpSimpleFn :: Text -> Exp -> Exp_
pattern ExpSimpleFn vid exp <- (expSimpleFn -> Just (vid, exp))
expSimpleFn :: Exp_ -> Maybe (Text, Exp)
expSimpleFn (ExpFn (At _ (Match [(p, e)]))) = (,e) <$> go p
    where go (At _ (PatParen p)) = go p
          go (At _ (PatFlatApp [p])) = go p
          go (At s (PatVar _ (At _ (Vid vid)))) = Just vid
expSimpleFn _ = Nothing

data SimpleType
    = STVar !Int
    | STPrim !Text
    | STFunc !SimpleType !SimpleType
    | STRecord !(Record SimpleType)

data VarState = VarState
    { _varState_lowerBounds :: !(Set SimpleType)
    , _varState_upperBounds :: !(Set SimpleType)
    }

data Env = Env
    { _env_bindings     :: !(Map Text SimpleType)
    , _env_fixities     :: !(Map Text Fixity)
    , _env_constructors :: !(Set Text)
    }

data VarEnv = VarEnv
    { _varEnv_varStates :: !(IntMap VarState)
    , _varEnv_nextId    :: !Int
    }

initialVarState :: VarState
initialVarState = VarState
    { _varState_lowerBounds = Set.empty
    , _varState_upperBounds = Set.empty
    }

initialEnv :: Env
initialEnv = Env
    { _env_bindings = Map.empty
    , _env_fixities = Map.empty
    , _env_constructors = Set.empty
    }

initialVarEnv :: VarEnv
initialVarEnv = VarEnv
    { _varEnv_varStates = IntMap.empty
    , _varEnv_nextId = 0
    }

runTC :: TC a -> Span -> (Either CompilerError (a, VarEnv), Seq CompilerWarning)
runTC (TC m) span = m
    `runReaderT` initialEnv
    `runLocatedT` span
    `runStateT` initialVarEnv
    & runWarn

newtype TC a = TC
    { unTC :: ReaderT Env (LocatedT (StateT VarEnv (Warn CompilerWarning CompilerError))) a
    }
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadReader Env
                     , MonadState VarEnv
                     , MonadWarn CompilerWarning CompilerError
                     , MonadLocated
                     )

-- | Create a fresh variable with no upper/lower bounds
freshVar :: TC SimpleType
freshVar = do
    VarEnv varStates nextId <- get
    put $ VarEnv (IntMap.insert nextId initialVarState varStates) (nextId + 1)
    pure $ STVar nextId

-- | Signal a type error
typeError :: Text -> TC a
typeError msg = compilerExternalError $ "Type Error: " <> vacuous (pretty msg)

-- | Determine the type of an expression
typeExp :: Exp -> TC SimpleType
typeExp (At s e) =
  withCurrentSpan s $
    case e of
      ExpConst (At s k) ->
        withCurrentSpan s $
          case k of
            ConstBool _   -> pure $ STPrim "bool"
            ConstChar _   -> pure $ STPrim "char"
            ConstInt _    -> pure $ STPrim "int"
            ConstReal _   -> pure $ STPrim "real"
            ConstString _ -> pure $ STPrim "string"
            ConstWord _   -> pure $ STPrim "word"
      ExpVar _ (At _ (Vid vid)) -> do
          bindings <- asks _env_bindings
          case Map.lookup vid bindings of
            Nothing -> typeError $ "Identifier \"" <> vid <> "\" is undefined"
            Just t -> pure t
      ExpRecord r -> STRecord <$> traverse typeExp r
      ExpSimpleFn param body -> do
          paramTy <- freshVar
          bodyTy <- local
              (\env -> env { _env_bindings = Map.insert param paramTy (_env_bindings env) })
              (typeExp body)
          pure $ STFunc paramTy bodyTy
      ExpFlatApp es -> undefined
      ExpApp e1 e2 -> do
          resTy <- freshVar
          e1Ty <- typeExp e1
          e2Ty <- typeExp e2
          e1Ty <: STFunc e2Ty resTy
          pure resTy
      ExpGet obj fld -> do
          resTy <- freshVar
          objTy <- typeExp obj
          objTy <: STRecord (Record [(fld, resTy)])
          pure resTy
      ExpSelector fld -> do
          fldTy <- freshVar
          pure $ STFunc (STRecord (Record [(fld, fldTy)])) fldTy
      _ -> undefined

-- | Constrain the types so the left is a subtype of the right
(<:) :: SimpleType -> SimpleType -> TC ()
(<:) = undefined

