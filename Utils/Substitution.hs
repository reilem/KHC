{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Utils.Substitution where

import Frontend.HsTypes
import Backend.FcTypes

import Utils.Var
import Utils.Kind
import Utils.Annotated
import Utils.Unique
import Utils.Utils
import Utils.PrettyPrint

import Control.Monad (liftM2, foldM)

-- * The SubstVar Class
-- ------------------------------------------------------------------------------

class SubstVar v t x | v x -> t where -- The FD means that we can not use the class for renaming substitutions.
  substVar :: v -> t -> x -> x

instance SubstVar v t x => SubstVar v t [x] where
  substVar v t xs = map (substVar v t) xs

-- * Source Language SubstVar Instances (Type Substitution)
-- ------------------------------------------------------------------------------

-- | Substitute a type variable for a type in a type
instance SubstVar RnTyVar RnMonoTy RnMonoTy where
  substVar a ty = \case
    TyVar b
      | a == b    -> ty
      | otherwise -> TyVar b
    TyCon tc      -> TyCon tc
    TyApp ty1 ty2 -> TyApp (substVar a ty ty1) (substVar a ty ty2)

-- | Substitute a type variable for a type in a class constraint
instance SubstVar RnTyVar RnMonoTy RnClsCt where
  substVar a ty (ClsCt cls mono) = ClsCt cls (substVar a ty mono)

-- | Substitute a type variable for a type in a qualified type
instance SubstVar RnTyVar RnMonoTy RnQualTy where
  substVar a aty = \case
    QMono    ty -> QMono (substVar a aty ty)
    QQual ct ty -> QQual (substVar a aty ct) (substVar a aty ty)

-- | Substitute a type variable for a type in a type scheme
instance SubstVar RnTyVar RnMonoTy RnPolyTy where
  substVar a aty = \case
    PQual ty      -> PQual (substVar a aty ty)
    PPoly (b :| kind) ty
      | a == b    -> error "substTyVarInPolyTy: Shadowing"
      | otherwise -> PPoly (b :| kind) (substVar a aty ty)

-- | Substitute a type variable for a type in a constraint
instance SubstVar RnTyVar RnMonoTy RnCtr where
  substVar a ty = \case
    Ctr as cs ct
      | elem a (map labelOf as) -> error "substTyVarInCtr: Shadowing"
      | otherwise -> Ctr as (map (substVar a ty) cs) (substVar a ty ct)

instance SubstVar RnTmVar RnTmVar RnPat where
  substVar a ax = \case
    HsVarPat x     -> HsVarPat x
    HsConPat dc ps -> HsConPat dc (map (substVar a ax) ps)
    HsOrPat  p1 p2 -> HsOrPat (substVar a ax p1) (substVar a ax p2)
    HsWildPat      -> HsWildPat

-- * Target Language SubstVar Instances (Type Substitution)
-- ------------------------------------------------------------------------------

-- | Substitute a type variable for a type in a type
instance SubstVar FcTyVar FcType FcType where
  substVar a ty = \case
    FcTyVar b
      | a == b      -> ty
      | otherwise   -> FcTyVar b
    FcTyAbs b ty1
      | a == b      -> error "substFcVarInFcType: Shadowing (tyabs)"
      | otherwise   -> FcTyAbs b (substVar a ty ty1)
    FcTyApp ty1 ty2 -> FcTyApp (substVar a ty ty1) (substVar a ty ty2)
    FcTyCon tc      -> FcTyCon tc

-- | Substitute a type variable for a type in a term
instance SubstVar FcTyVar FcType (FcTerm a) where
  substVar a aty = \case
    FcTmVar x            -> FcTmVar x
    FcTmAbs x ty tm      -> FcTmAbs x (substVar a aty ty) (substVar a aty tm)
    FcTmApp tm1 tm2      -> FcTmApp (substVar a aty tm1) (substVar a aty tm2)
    FcTmTyAbs b tm
      | a == b           -> error "substFcTyVarInTm: Shadowing (tyabs)"
      | otherwise        -> FcTmTyAbs b (substVar a aty tm)
    FcTmTyApp tm ty      -> FcTmTyApp (substVar a aty tm) (substVar a aty ty)
    FcTmDataCon dc       -> FcTmDataCon dc
    FcTmLet x ty tm1 tm2 -> FcTmLet x (substVar a aty ty) (substVar a aty tm1) (substVar a aty tm2)
    FcTmCaseFc tm cs     -> FcTmCaseFc (substVar a aty tm) (map (substVar a aty) cs)
    FcTmCaseTc ty1 ty2 tm cs -> FcTmCaseTc (substVar a aty ty1) (substVar a aty ty2) (substVar a aty tm) (map (substVar a aty) cs)
    FcTmERROR s ty       -> FcTmERROR s (substVar a aty ty)

-- | Substitute a type variable for a type in a case alternative
instance SubstVar FcTyVar FcType (FcAlt a) where
  substVar a ty (FcAltFc p tm)  = FcAltFc p (substVar a ty tm)
  -- GEORGE: Now the patterns do not bind type variables so we don't have to check for shadowing here.
  substVar a ty (FcAltTc p gRs) = FcAltTc p (map (substVar a ty) gRs)

instance SubstVar FcTyVar FcType (FcGuarded a) where
  substVar a ty (FcGuarded gs tm) = FcGuarded gs (substVar a ty tm)

-- * Target Language SubstVar Instances (Term Substitution)
-- ------------------------------------------------------------------------------

-- | Substitute a term variable for a term in a term
instance SubstVar FcTmVar (FcTerm a) (FcTerm a) where
  substVar x xtm = \case
    FcTmVar y
      | x == y       -> xtm
      | otherwise    -> FcTmVar y
    FcTmAbs y ty tm
      | x == y       -> error "substFcTmVarInTm: Shadowing (tmabs)"
      | otherwise    -> FcTmAbs y ty (substVar x xtm tm)
    FcTmApp tm1 tm2  -> FcTmApp (substVar x xtm tm1) (substVar x xtm tm2)

    FcTmTyAbs a tm   -> FcTmTyAbs a (substVar x xtm tm)
    FcTmTyApp tm ty  -> FcTmTyApp (substVar x xtm tm) ty
    FcTmDataCon dc   -> FcTmDataCon dc
    FcTmLet y ty tm1 tm2
      | x == y       -> error "substFcTmVarInTm: Shadowing (let)"
      | otherwise    -> FcTmLet y ty (substVar x xtm tm1) (substVar x xtm tm2)
    FcTmCaseFc tm cs -> FcTmCaseFc (substVar x xtm tm) (map (substVar x xtm) cs)
    FcTmCaseTc ty1 ty2 tm cs -> FcTmCaseTc ty1 ty2 (substVar x xtm tm) (map (substVar x xtm) cs)
    FcTmERROR s ty   -> FcTmERROR s ty

-- | Substitute a term variable for a term in a case alternative
instance SubstVar FcTmVar (FcTerm a) (FcAlt a) where
  substVar x xtm (FcAltFc p tm)  = FcAltFc (substVar x xtm p) (substVar x xtm tm)
  substVar x xtm (FcAltTc p gRs) = FcAltTc (substVar x xtm p) (map (substVar x xtm) gRs)

instance SubstVar FcTmVar (FcTerm a) (FcGuarded a) where
  substVar x xtm (FcGuarded gs tm) = FcGuarded (map (substVar x xtm) gs) (substVar x xtm tm)

instance SubstVar FcTmVar (FcTerm a) (FcGuard a) where
  substVar x xtm (FcPatGuard p tm) = FcPatGuard (substVar x xtm p) (substVar x xtm tm)

-- | Substitute a term variable for a term in a pattern
instance SubstVar FcTmVar (FcTerm a) (FcPat a) where
  substVar x _ (FcConPat dc xs)
    | not (distinct xs) = error "substFcTmVarInAlt: Variables in pattern are not distinct" -- extra redundancy for safety
    | any (==x) xs      = error "substFcTmVarInPat: Shadowing"
    | otherwise         = (FcConPat dc xs)
  substVar x _ (FcVarPat y)
    | x == y            = error "substFcTmVarInPat: Shadowing" -- extra redundancy for safety
    | otherwise         = (FcVarPat y)
  substVar x xtm (FcConPatNs dc ps) = FcConPatNs dc (substVar x xtm ps)
  substVar x xtm (FcOrPat    p1 p2) = FcOrPat (substVar x xtm p1) (substVar x xtm p2)

-- ------------------------------------------------------------------------------

-- | General structure of substitutions as snoc lists
data Sub x y = SNil | SCons (Sub x y) x y

mapSubM :: Monad m => (x -> m x') -> (y -> m y') -> Sub x y -> m (Sub x' y')
mapSubM _fx _fy SNil          = return SNil
mapSubM  fx  fy (SCons s x y) = do
  s' <- mapSubM fx fy s
  x' <- fx x
  y' <- fy y
  return (SCons s' x' y')

instance (PrettyPrint x, PrettyPrint y) => PrettyPrint (Sub x y) where
  ppr = brackets . sep . punctuate comma. reverse . to_doc_list
    where
      to_doc_list SNil          = []
      to_doc_list (SCons s x y) = (ppr x <+> text "|->" <+> ppr y) : to_doc_list s

  needsParens _ = False

instance Semigroup (Sub x y) where
  (<>) sub SNil          = sub
  (<>) sub (SCons s x y) = SCons (sub <> s) x y

instance Monoid (Sub x y) where
  mempty = SNil
  mconcat = foldl mappend SNil -- foldl since mappend does induction on the second argument

instance Subst (Sub x y) x y where
  (|->) x y = SCons SNil x y

-- | GEORGE: DOCUMENT ME
sub_rec :: SubstVar v t x => Sub v t -> x -> x
sub_rec SNil          t = t
sub_rec (SCons s x y) t = sub_rec s (substVar x y t)

-- * The ApplySubst Class
-- ------------------------------------------------------------------------------

class ApplySubst s t where
  applySubst :: s -> t -> t

instance ApplySubst s t => ApplySubst s [t] where
  applySubst s xs = map (applySubst s) xs

instance SubstVar v t x => ApplySubst (Sub v t) x where
  applySubst = sub_rec

-- * Type Substitution (Source Language)
-- ------------------------------------------------------------------------------

type HsTySubst = Sub RnTyVar RnMonoTy

-- | Build a type substitution from an association list between type variables
buildRnSubst :: [(RnTyVar, RnTyVar)] -> HsTySubst
buildRnSubst = foldl (\s (x,y) -> SCons s x (TyVar y)) SNil

-- | Apply a type substitution to a monotype
substInMonoTy :: HsTySubst -> RnMonoTy -> RnMonoTy
substInMonoTy = sub_rec

-- | Apply a type substitution to a type equality
substInEqCt :: HsTySubst -> EqCt -> EqCt
substInEqCt subst (ty1 :~: ty2) = substInMonoTy subst ty1 :~: substInMonoTy subst ty2

-- | Apply a type substitution to a a list of type equalities
substInEqCs :: HsTySubst -> EqCs -> EqCs
substInEqCs subst = map (substInEqCt subst)

-- | Apply a type substitution to a constraint
substInCtr :: HsTySubst -> RnCtr -> RnCtr
substInCtr = sub_rec

-- | Apply a type substitution to a list of constraints
substInCts :: HsTySubst -> RnCts -> RnCts
substInCts subst = map (substInCtr subst)

-- | Apply a type substitution to a class constraint
substInClsCt :: HsTySubst -> RnClsCt -> RnClsCt
substInClsCt subst (ClsCt cls ty) = ClsCt cls (substInMonoTy subst ty)

-- | Apply a type substitution to a list of class constraints
substInClsCs :: HsTySubst -> RnClsCs -> RnClsCs
substInClsCs subst = map (substInClsCt subst)

-- | Apply a type substitution to a type variable
substInTyVar :: HsTySubst -> RnTyVar -> RnMonoTy
substInTyVar subst tv = substInMonoTy subst (TyVar tv)

-- | Apply a type substitution to a list of type variables
substInTyVars :: HsTySubst -> [RnTyVar] -> [RnMonoTy]
substInTyVars subst = map (substInTyVar subst)

-- | Apply a type substitution to a program theory
substInProgramTheory :: HsTySubst -> ProgramTheory -> ProgramTheory
substInProgramTheory subst = fmap (\(d :| ct) -> (d :| substInCtr subst ct))

-- | Apply a type substitution to a simple program theory
substInSimpleProgramTheory :: HsTySubst -> SimpleProgramTheory -> SimpleProgramTheory
substInSimpleProgramTheory subst = fmap (\(d :| ct) -> (d :| substInClsCt subst ct))

-- | Apply a type substitution to a qualified type
substInQualTy :: HsTySubst -> RnQualTy -> RnQualTy
substInQualTy = sub_rec

-- | Apply a type substitution to a type scheme
substInPolyTy :: HsTySubst -> RnPolyTy -> RnPolyTy
substInPolyTy = sub_rec

-- * Term Substitution (Source Language)
-- ------------------------------------------------------------------------------

type HsTmSubst = Sub RnTmVar RnTmVar

buildRnTmSubst :: [(RnTmVar, RnTmVar)] -> HsTmSubst
buildRnTmSubst = foldl (\s (x,y) -> SCons s x y) SNil

-- | Apply a list of term substitutions to a pattern
substInPat :: HsTmSubst -> RnPat -> RnPat
substInPat = sub_rec

-- | Replaces renamed pattern variables in a pattern according to given substition.
replacePatBinds :: [(RnTmVar, RnTmVar)] -> RnPat -> RnPat
replacePatBinds []          = id
replacePatBinds ((b,bx):bs) = replacePatBinds bs . go b bx
  where
    go :: RnTmVar -> RnTmVar -> RnPat -> RnPat
    go a ax = \case
      HsVarPat x
        | a == x     -> HsVarPat ax
        | otherwise  -> HsVarPat x
      HsConPat dc ps -> HsConPat dc (map (go a ax) ps)
      HsOrPat  p1 p2 -> HsOrPat (go a ax p1) (go a ax p2)
      HsWildPat      -> HsWildPat

-- * System F Type Substitution
-- ------------------------------------------------------------------------------

type FcTySubst = Sub FcTyVar FcType

-- | Apply a type substitution to a type
substFcTyInTy :: FcTySubst -> FcType -> FcType
substFcTyInTy = sub_rec

-- | Apply a type substitution to a term
substFcTyInTm :: FcTySubst -> (FcTerm a) -> (FcTerm a)
substFcTyInTm = sub_rec

-- | Apply a type substitution to a case alternative
substFcTyInAlt :: FcTySubst -> (FcAlt a) -> (FcAlt a)
substFcTyInAlt = sub_rec -- XXX: subst (FcAlt p tm) = FcAlt p (substFcTyInTm subst tm)
  -- GEORGE: Now the patterns do not bind type variables so we don't have to check for shadowing here.

-- * System F Term Substitution
-- ------------------------------------------------------------------------------

type FcTmSubst a = Sub FcTmVar (FcTerm a)

-- | Apply a term substitution to a term
substFcTmInTm :: (FcTmSubst a) -> (FcTerm a) -> (FcTerm a)
substFcTmInTm = sub_rec

-- | Apply a term substitution to a case alternative
substFcTmInAlt :: (FcTmSubst a) -> (FcAlt a) -> (FcAlt a)
substFcTmInAlt = sub_rec

-- * The Subst class
-- ------------------------------------------------------------------------------

class Monoid s => Subst s dom img | s -> dom img, dom img -> s where
  (|->)   :: dom -> img -> s

-- * Alpha-equality on System F Types
-- ------------------------------------------------------------------------------

-- | Alpha equality on types
alphaEqFcTypes :: MonadUnique m => FcType -> FcType -> m Bool
alphaEqFcTypes (FcTyVar a)       (FcTyVar b)       = return (a == b)
alphaEqFcTypes (FcTyAbs a ty1) (FcTyAbs b ty2) = do
  -- GEORGE: Do we need to check that the kinds are the same?
  -- Need to go over the implementation at some point soon.
  c <- FcTyVar <$> freshFcTyVar (kindOf a)
  let ty1' = substFcTyInTy (a |-> c) ty1
  let ty2' = substFcTyInTy (b |-> c) ty2
  alphaEqFcTypes ty1' ty2'
alphaEqFcTypes (FcTyApp ty1 ty2) (FcTyApp ty3 ty4) = liftM2 (&&) (alphaEqFcTypes ty1 ty3) (alphaEqFcTypes ty2 ty4)
alphaEqFcTypes (FcTyCon tc1)     (FcTyCon tc2)     = return (tc1 == tc2)

alphaEqFcTypes (FcTyVar {}) _ = return False
alphaEqFcTypes (FcTyAbs {}) _ = return False
alphaEqFcTypes (FcTyApp {}) _ = return False
alphaEqFcTypes (FcTyCon {}) _ = return False

-- * Freshen up all local binders
-- ------------------------------------------------------------------------------

class FreshenLclBndrs a where
  freshenLclBndrs :: MonadUnique m => a -> m a

-- | Freshen the (type) binders of a type scheme
instance FreshenLclBndrs RnPolyTy where
  freshenLclBndrs (PQual ty) = return (PQual ty)
  freshenLclBndrs (PPoly (a :| _) ty) = freshRnTyVar (kindOf a) >>= \b ->
    PPoly (b :| kindOf b) <$> freshenLclBndrs (substVar a (TyVar b) ty)

-- | Freshen the (type) binders of a constraint
instance FreshenLclBndrs RnCtr where
  freshenLclBndrs (Ctr []     cs ct) = return (Ctr [] cs ct)
  freshenLclBndrs (Ctr ((a :| _):as) cs ct) = do
    b                <- freshRnTyVar (kindOf a)
    (Ctr bs cs' ct') <- freshenLclBndrs (substVar a (TyVar b) $ Ctr as cs ct)
    return (Ctr ((b :| kindOf b) : bs) cs' ct')

-- | Freshen the (type) binders of a System F type
instance FreshenLclBndrs FcType where
  freshenLclBndrs (FcTyVar a)       = return (FcTyVar a)
  freshenLclBndrs (FcTyAbs a ty)    = freshFcTyVar (kindOf a) >>= \b ->
    FcTyAbs b <$> freshenLclBndrs (substVar a (FcTyVar b) ty)
  freshenLclBndrs (FcTyApp ty1 ty2) = FcTyApp <$> freshenLclBndrs ty1 <*> freshenLclBndrs ty2
  freshenLclBndrs (FcTyCon tc)      = return (FcTyCon tc)

-- | Freshen the (type + term) binders of a System F term
instance FreshenLclBndrs (FcTerm a) where
  freshenLclBndrs (FcTmAbs x ty tm) = freshFcTmVar >>= \y ->
    FcTmAbs y <$> freshenLclBndrs ty <*> freshenLclBndrs (substVar x (FcTmVar y) tm)
  freshenLclBndrs (FcTmVar x)       = return (FcTmVar x)
  freshenLclBndrs (FcTmApp tm1 tm2) = FcTmApp <$> freshenLclBndrs tm1 <*> freshenLclBndrs tm2
  freshenLclBndrs (FcTmTyAbs a tm)  = freshFcTyVar (kindOf a) >>= \b ->
    FcTmTyAbs b <$> freshenLclBndrs (substVar a (FcTyVar b) tm)
  freshenLclBndrs (FcTmTyApp tm ty) = FcTmTyApp <$> freshenLclBndrs tm <*> freshenLclBndrs ty
  freshenLclBndrs (FcTmDataCon dc)  = return (FcTmDataCon dc)
  freshenLclBndrs (FcTmLet x ty tm1 tm2) = freshFcTmVar >>= \y ->
    FcTmLet y <$> freshenLclBndrs ty
              <*> freshenLclBndrs (substVar x (FcTmVar y) tm1)
              <*> freshenLclBndrs (substVar x (FcTmVar y) tm2)

  freshenLclBndrs (FcTmCaseFc tm cs) = FcTmCaseFc <$> freshenLclBndrs tm <*> mapM freshenLclBndrs cs
  freshenLclBndrs (FcTmCaseTc ty1 ty2 tm cs) = FcTmCaseTc <$> freshenLclBndrs ty1 <*> freshenLclBndrs ty2 <*> freshenLclBndrs tm <*> mapM freshenLclBndrs cs
  freshenLclBndrs (FcTmERROR s ty)   = FcTmERROR s <$> freshenLclBndrs ty

-- | Freshen the (type + term) binders of a System F case alternative
instance FreshenLclBndrs (FcAlt a) where
  freshenLclBndrs (FcAltFc p tm) = do
    (p', substs) <- freshenPatLclBndrs p
    tm' <- freshenLclBndrs (foldr (\(a,b) acc -> substVar a (FcTmVar b) acc) tm substs)
    return (FcAltFc p' tm')

freshenPatLclBndrs :: MonadUnique m => FcPat a -> m (FcPat a, [(FcTmVar, FcTmVar)])
freshenPatLclBndrs (FcConPat dc xs) = do
  ys  <- mapM (\_ -> freshFcTmVar) xs
  return (FcConPat dc ys, zip xs ys)
freshenPatLclBndrs (FcVarPat x) = do
  y <- freshFcTmVar
  return (FcVarPat y, [(x, y)])
freshenPatLclBndrs (FcConPatNs dc ps) = do
  (ps', substs) <- foldM freshenPat ([], []) ps
  return (FcConPatNs dc ps', substs)
  where
    freshenPat :: MonadUnique m => ([FcPat a], [(FcTmVar, FcTmVar)]) -> FcPat a -> m ([FcPat a], [(FcTmVar, FcTmVar)])
    freshenPat (ps', substs') p = do
      (p', substs'') <- freshenPatLclBndrs p
      return (ps' ++ [p'], substs' ++ substs'')
freshenPatLclBndrs (FcOrPat p1 p2) = do
  (p1', substs) <- freshenPatLclBndrs p1
  let p2' = applySubsts substs p2
  return (FcOrPat p1' p2', substs)
  where
    -- | This is very inefficient, N^2 for FcConPat
    applySubsts :: [(FcTmVar, FcTmVar)] -> FcPat a -> FcPat a
    applySubsts []          p = p
    applySubsts ((x, y):xs) p = apply x y (applySubsts xs p)
    apply :: FcTmVar -> FcTmVar -> FcPat a -> FcPat a
    apply a b (FcConPat dc xs)   = FcConPat dc (map (\x -> if a == x then b else x) xs)
    apply a b (FcVarPat x)
      | a == x                   = FcVarPat b
      | otherwise                = FcVarPat x
    apply a b (FcConPatNs dc ps) = FcConPatNs dc (map (apply a b) ps)
    apply a b (FcOrPat p11 p12)  = FcOrPat (apply a b p11) (apply a b p12)
