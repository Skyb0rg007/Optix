
{-# LANGUAGE RecordWildCards #-}

module Language.Optix.Typecheck.Scope (scope) where

import           Control.Monad.State
import           Control.Monad.Warn
import           Data.Foldable       (foldl', foldlM)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Void           (vacuous)
import           Debug.Trace

import           Language.Optix.Frontend.Syntax
import           Language.Optix.Util.Error
import           Language.Optix.Util.Located
import           Language.Optix.Util.Pretty

data PD m up down = PD
    { bindType :: down -> [TyVar] -> (down, up -> up)
    , bindFunVal :: down -> [TyVar] -> Span -> (down, up -> m ([TyVar], up))
    , combineUp :: up -> up -> up
    , initDown :: down
    , initUp :: up
    , tyvar :: TyVar -> down -> up
    }

processDec
    :: forall m up down. Monad m
    => PD m up down
    -> Dec
    -> m (Dec, up)
processDec PD { .. } d = loopDec d initDown
    where
        visits :: forall a. [a] -> (a -> up) -> up
        visits xs visitX = foldl' (\u x -> combineUp u (visitX x)) initUp xs
        visitTy :: Type -> down -> up
        visitTy t d = visit t
            where
                visit (At _ ty) =
                    case ty of
                      TypeCon _ ts -> visits ts visit
                      TypeParen t -> visit t
                      TypeRecord r -> foldl' (\u t -> combineUp u (visit t)) initUp r
                      TypeVar tv -> tyvar tv d
        visitTyOpt :: Maybe Type -> down -> up
        visitTyOpt Nothing _ = initUp
        visitTyOpt (Just t) d = visitTy t d
        visitTypBind :: TypBind -> down -> up
        visitTypBind (At _ (TypBind tbs)) d =
            visits tbs $ \(TypBindNode _tycon def tyvars) ->
                let (d', finish) = bindType d tyvars
                 in finish (visitTy def d')
        visitDatBind :: DatBind -> down -> up
        visitDatBind (At _ (DatBind datatypes withtypes)) d =
            let u = visits datatypes $ \(DatBindDatatype cons tycons tyvars) ->
                    let (d', finish) = bindType d tyvars
                     in finish (visits cons $ \(_, arg) -> visitTyOpt arg d')
                u' = visitTypBind withtypes d
             in combineUp u u'
        visitPat :: Pat -> down -> up
        visitPat p d = visit p
            where
                visit (At _ pat) =
                    case pat of
                      PatApp _ p -> visit p
                      PatConst _ -> initUp
                      PatConstraint p t ->
                          combineUp (visit p) (visitTy t d)
                      PatFlatApp ps -> visits ps visit
                      PatLayered _ _ mTy pat ->
                          combineUp (visitTyOpt mTy d) (visit pat)
                      PatList ps -> visits ps visit
                      PatOr ps -> visits ps visit
                      PatParen p -> visit p
                      PatRecord prfs ->
                          foldl'
                          (\u (At _ prf) -> 
                              case prf of
                                PatRecordFieldField _ p -> visit p
                                PatRecordFieldVid _ _ mTy mPat ->
                                    let u = visitTyOpt mTy d
                                        u' = visitOpt mPat
                                     in combineUp u u')
                          initUp
                          prfs
                      PatTuple ps -> visits ps visit
                      PatVar _ _ -> initUp
                      PatVector ps -> visits ps visit
                      PatWild -> initUp
                visitOpt Nothing = initUp
                visitOpt (Just p) = visit p
        loops :: forall a. [a] -> (a -> m (a, up)) -> m ([a], up)
        loops xs loopX =
            foldlM
            (\(out, u) x -> do
                (x', u') <- loopX x
                pure (x' : out, combineUp u u'))
            ([], initUp)
            xs
        loopDec :: Dec -> down -> m (Dec, up)
        loopDec dec@(At s d) down =
            case d of
              DecDatatype datBind -> 
                  let u = visitDatBind datBind down
                   in pure (dec, u)
              DecDo exp -> do
                  (e, u) <- loopExp exp down
                  pure (At s (DecDo e), u)
              DecFix {} -> pure (dec, initUp)
              DecFun tyvars funBinds -> do
                  let (down, finish) = bindFunVal down tyvars s
                  (fbs, u) <- loops funBinds $ \funClauses ->
                      loops funClauses $ \(At s (FunBind body pats resTy)) -> do
                          (body, u) <- loopExp body down
                          let u' = visits pats $ \p -> visitPat p down
                          let u'' = visitTyOpt resTy down
                          pure (At s (FunBind body pats resTy), combineUp u (combineUp u' u''))
                  (tyvars', u') <- finish u
                  pure (At s (DecFun tyvars funBinds), u')
              DecLocal d1 d2 -> do
                  (d1', u1) <- loops d1 $ \d -> loopDec d down
                  (d2', u2) <- loops d2 $ \d -> loopDec d down
                  pure (At s (DecLocal d1' d2'), combineUp u1 u2)
              DecType typBind -> pure (dec, visitTypBind typBind down)
              DecVal tyvars valBinds -> do
                  let (down, finish) = bindFunVal down tyvars s
                  (vbs, u) <-
                      loops valBinds $ \(pat, exp) -> do
                          (exp', u) <- loopExp exp down
                          let u' = visitPat pat down
                          pure ((pat, exp'), combineUp u u')
                  (tvs, u') <- finish u
                  pure (At s (DecVal tvs vbs), u')
        loopExp :: Exp -> down -> m (Exp, up)
        loopExp e d = loop e
            where
                loop :: Exp -> m (Exp, up)
                loop exp@(At s e) =
                    case e of
                      ExpAndAlso e1 e2 -> do
                          (e1', u1) <- loop e1
                          (e2', u2) <- loop e2
                          pure (At s (ExpAndAlso e1' e2'), combineUp u1 u2)
                      ExpApp e1 e2 -> do
                          (e1', u1) <- loop e1
                          (e2', u2) <- loop e2
                          pure (At s (ExpApp e1' e2'), combineUp u1 u2)
                      ExpCase e m -> do
                          (e', u1) <- loop e
                          (m', u2) <- loopMatch m d
                          pure (At s (ExpCase e' m'), combineUp u1 u2)
                      ExpConst {} -> pure (exp, initUp)
                      ExpConstraint e t -> do
                          (e', u) <- loop e
                          let u' = visitTy t d
                          pure (At s (ExpConstraint e' t), combineUp u u')
                      ExpFlatApp es -> do
                          (es', u) <- loops es loop
                          pure (At s (ExpFlatApp es'), u)
                      ExpFn m -> do
                          (m', u) <- loopMatch m d
                          pure (At s (ExpFn m'), u)
                      ExpIf e1 e2 e3 -> do
                          (e1', u1) <- loop e1
                          (e2', u2) <- loop e2
                          (e3', u3) <- loop e3
                          pure (At s (ExpIf e1' e2' e3'), combineUp u1 (combineUp u2 u3))
                      ExpLet decs e -> do
                          (decs', u) <- loops decs $ \dec -> loopDec dec d
                          (e', u') <- loop e
                          pure (At s (ExpLet decs' e'), combineUp u u')
                      ExpList es -> do
                          (es', u) <- loops es loop
                          pure (At s (ExpList es'), u)
                      ExpOrElse e1 e2 -> do
                          (e1', u1) <- loop e1
                          (e2', u2) <- loop e2
                          pure (At s (ExpOrElse e1' e2'), combineUp u1 u2)
                      ExpParen e -> do
                          (e', u) <- loop e
                          pure (At s (ExpParen e'), u)
                      ExpRecord r -> do
                          let change :: Record a -> ([a] -> m ([b], c)) -> m (Record b, c) 
                              change (Record flds) f = do
                                  let (fs, xs) = unzip flds
                                  (ys, c) <- f xs
                                  pure (Record (zip fs ys), c)
                          (r, u) <- change r (\res -> loops res loop) {- HLINT ignore -}
                          pure (At s (ExpRecord r), u)
                      ExpSelector _ -> pure (exp, initUp)
                      ExpGet e fld -> do
                          (e', u) <- loop e
                          pure (At s (ExpGet e' fld), u)
                      ExpSequence es -> do
                          (es', u) <- loops es loop
                          pure (At s (ExpSequence es'), u)
                      ExpVar {} -> pure (exp, initUp)
                      ExpVector es -> do
                          (es', u) <- loops es loop
                          pure (At s (ExpVector es'), u)
                      ExpWhile test expr -> do
                          (test', u1) <- loop test
                          (expr', u2) <- loop expr
                          pure (At s (ExpWhile test' expr'), combineUp u1 u2)
        loopMatch :: Match -> down -> m (Match, up)
        loopMatch (At s (Match rules)) d = do
            (rules', u) <-
                loops rules $ \(p, e) -> do
                    let u = visitPat p d
                    (e', u') <- loopExp e d
                    pure ((p, e'), combineUp u u')
            pure (At s (Match rules'), u)

data Up1 = Up1
    { _up1_free :: Set TyVar
    , _up1_mayNotBind :: [TyVar]
    }
type Down1 = ()

type Up2 = ()
type Down2 = Set TyVar

scope
    :: forall m.
    ( MonadLocated m
    , MonadWarn CompilerWarning CompilerError m )
    => Dec
    -> m Dec
scope dec = do
    traceM "starting scope"
    (dec', _) <- processDec pd1 dec
    traceM "process1 complete"
    (dec'', _) <- processDec pd2 dec
    traceM "process2 complete"
    pure dec''
    where
        bindType1 :: Down1 -> [TyVar] -> (Down1, Up1 -> Up1)
        bindType1 () tyvars = ((), finish)
            where
                finish :: Up1 -> Up1
                finish (Up1 free _) = Up1 (free Set.\\ Set.fromList tyvars) []
        bindFunVal1 :: Down1 -> [TyVar] -> Span -> (Down1, Up1 -> m ([TyVar], Up1))
        bindFunVal1 () tyvars span = ((), finish)
            where
                finish :: Up1 -> m ([TyVar], Up1)
                finish (Up1 free mayNotBind) = do
                    let bound = Set.union free (Set.fromList tyvars)
                    mayNotBind' <- flip filterM mayNotBind $ \a ->
                            if a `Set.notMember` bound
                               then pure True
                               else do
                                   withCurrentSpan span $
                                       compilerExternalWarning $ vsep
                                           [ "type variable scoped at an outer declaration: " <> fmap ansiStyle (pretty a)
                                           , "scoped at: " <> vacuous (pretty span)
                                           ]
                                   pure False
                    pure (Set.toList bound, Up1 Set.empty (tyvars ++ mayNotBind'))
        tyvar1 :: TyVar -> Down1 -> Up1
        tyvar1 a () = Up1 (Set.singleton a) []
        combineUp1 :: Up1 -> Up1 -> Up1
        combineUp1 (Up1 f m) (Up1 f' m') = Up1 (Set.union f f') (m ++ m')
        pd1 :: PD m Up1 Down1
        pd1 = PD
            { bindType = bindType1
            , bindFunVal = bindFunVal1
            , combineUp = combineUp1
            , initDown = ()
            , initUp = Up1 { _up1_free = Set.empty, _up1_mayNotBind = [] }
            , tyvar = tyvar1
            }
        bindType2 :: Down2 -> [TyVar] -> (Down2, Up2 -> Up2)
        bindType2 bound tyvars = (Set.union bound (Set.fromList tyvars), \() -> ())
        bindFunVal2 :: Down2 -> [TyVar] -> Span -> (Down2, Up2 -> m ([TyVar], Up2))
        bindFunVal2 bound tyvars _ =
            let tyvars' = filter (\a -> a `Set.notMember` bound) tyvars
                bound' = Set.union bound (Set.fromList tyvars)
             in (bound, \() -> pure (tyvars, ()))
        tyvar2 :: TyVar -> Down2 -> Up2
        tyvar2 _ _ = ()
        pd2 :: PD m Up2 Down2
        pd2 = PD
            { bindType = bindType2
            , bindFunVal = bindFunVal2
            , combineUp = \() () -> ()
            , initDown = Set.empty
            , initUp = ()
            , tyvar = tyvar2
            }


