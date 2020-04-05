{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}

module Backend.FcTypeChecker (fcTypeCheck) where

import Backend.FcTypes

import Utils.Substitution
import Utils.Var
import Utils.Kind
import Utils.Unique
import Utils.AssocList
import Utils.Ctx
import Utils.PrettyPrint
import Utils.Errors
import Utils.Utils
import Utils.Trace

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Foldable (foldrM)

-- * Type checking monad
-- ----------------------------------------------------------------------------
type FcM = UniqueSupplyT (ReaderT FcCtx (StateT FcGblEnv (ExceptT String (Writer Trace))))

data FcGblEnv = FcGblEnv { fc_env_tc_info :: AssocList FcTyCon   FcTyConInfo
                         , fc_env_dc_info :: AssocList FcDataCon FcDataConInfo
                         }

instance PrettyPrint FcGblEnv where
  ppr (FcGblEnv tc_infos dc_infos)
    = braces $ vcat $ punctuate comma
    [ text "fc_env_tc_info" <+> colon <+> ppr tc_infos
    , text "fc_env_dc_info" <+> colon <+> ppr dc_infos ]
  needsParens _ = False

type FcCtx = Ctx FcTmVar FcType FcTyVar Kind

-- * Lookup things in the global environment
-- ----------------------------------------------------------------------------

-- | Lookup something in the global environment
lookupFcGblEnvM :: (Eq a, PrettyPrint a, MonadError String m, MonadState s m) => (s -> AssocList a b) -> a -> m b
lookupFcGblEnvM f x = gets f >>= \l -> case lookupInAssocList x l of
  Just y  -> return y
  Nothing -> throwErrorM (text "lookupFcGblEnvM" <+> colon <+> ppr x <+> text "is unbound")

-- | Lookup the info of a type constructor
lookupTyConInfoM :: FcTyCon -> FcM FcTyConInfo
lookupTyConInfoM = lookupFcGblEnvM fc_env_tc_info

-- | Lookup the kind of a type constructor
lookupTyConKindM :: FcTyCon -> FcM Kind
lookupTyConKindM tc = foldr KArr KStar . map kindOf . fc_tc_type_args <$> lookupTyConInfoM tc

-- | Lookup the info of a data constructor
lookupDataConInfoM :: FcDataCon -> FcM FcDataConInfo
lookupDataConInfoM = lookupFcGblEnvM fc_env_dc_info

-- | Lookup the type of a data constructor
lookupDataConTyM :: FcDataCon -> FcM ([FcTyVar], [FcType], FcTyCon)
lookupDataConTyM dc = lookupDataConInfoM dc >>= \info ->
  return (fc_dc_univ info, fc_dc_arg_tys info, fc_dc_parent info)

-- | Lookup the type constructor info given a certain data constructor
lookupDataConTyConInfoM :: FcDataCon -> FcM FcTyConInfo
lookupDataConTyConInfoM dc = do
  dc_info <- lookupDataConInfoM dc
  let ty_con = fc_dc_parent dc_info
  lookupTyConInfoM ty_con

-- * Ensure that some things are not bound in the local context
-- ----------------------------------------------------------------------------

-- | Ensure something is unbound in the local context
notInFcCtxM :: (PrettyPrint a, MonadReader ctx m, MonadError String m) => (ctx -> a -> Maybe t) -> a -> m ()
notInFcCtxM f x = ask >>= \ctx -> case f ctx x of
  Just {} -> throwErrorM (text "notInFcCtxM" <+> colon <+> ppr x <+> text "is already bound")
  Nothing -> return ()

-- | Ensure the type variable is not already bound
tyVarNotInFcCtxM :: FcTyVar -> FcM ()
tyVarNotInFcCtxM = notInFcCtxM lookupTyVarCtx

-- | Ensure the term variable is not already bound
tmVarNotInFcCtxM :: FcTmVar -> FcM ()
tmVarNotInFcCtxM = notInFcCtxM lookupTmVarCtx

-- | Ensure the list of term variables is not already bound
tmVarsNotInFcCtxM :: [FcTmVar] -> FcM ()
tmVarsNotInFcCtxM = mapM_ tmVarNotInFcCtxM

-- * Type checking
-- ----------------------------------------------------------------------------

mkDataConTy :: ([FcTyVar], [FcType], FcTyCon) -> FcType
mkDataConTy (as, arg_tys, tc) = fcTyAbs as $ fcTyArr arg_tys $ fcTyConApp tc (map FcTyVar as)

-- | Type check a data declaration
tcFcDataDecl :: FcDataDecl -> FcM ()
tcFcDataDecl (FcDataDecl _tc as dcs) = do
  forM_ as tyVarNotInFcCtxM  -- GEORGE: Ensure is not already bound
  forM_ dcs $ \(_dc, tys) -> do
    kinds <- extendCtxTysM as (map kindOf as) (mapM tcType tys)
    unless (all (==KStar) kinds) $
      throwError "tcFcDataDecl: Kind mismatch (FcDataDecl)"

-- | Type check a top-level value binding
tcFcValBind :: FcValBind a -> FcM (FcValBind 'Fc, FcCtx)
tcFcValBind (FcValBind x ty tm) = do
  tmVarNotInFcCtxM x  -- GEORGE: Ensure is not already bound
  kind <- tcType ty
  unless (kind == KStar) $
    throwError "tcFcValBind: Kind mismatch (FcValBind)"
  (fc_tm, ty') <- extendCtxTmM x ty (tcTerm tm)
  unless (ty `eqFcTypes` ty') $ throwErrorM (text "Global let type doesnt match:"
                                $$ parens (text "given:" <+> ppr ty)
                                $$ parens (text "inferred:" <+> ppr ty'))
  ctx <- extendCtxTmM x ty ask -- GEORGE: Return the extended environment
  return (FcValBind x ty fc_tm, ctx)

-- | Type check a program
tcFcProgram :: FcProgram a -> FcM (FcProgram 'Fc, FcType)
-- Type check a datatype declaration
tcFcProgram (FcPgmDataDecl datadecl pgm) = do
  tcFcDataDecl datadecl
  (fc_pgm, ty) <- tcFcProgram pgm
  return (FcPgmDataDecl datadecl fc_pgm, ty)
-- Type check a top-level value binding
tcFcProgram (FcPgmValDecl valbind pgm) = do
  (fc_valbind, fc_ctx) <- tcFcValBind valbind
  (fc_pgm, ty)         <- setCtxM fc_ctx $ tcFcProgram pgm
  return (FcPgmValDecl fc_valbind fc_pgm, ty)
-- Type check the top-level program expression
tcFcProgram (FcPgmTerm tm) = do
  (fc_tm, ty) <- tcTerm tm
  return (FcPgmTerm fc_tm, ty)

-- | Type check a System F term
tcTerm :: FcTerm a -> FcM (FcTerm 'Fc, FcType)
tcTerm (FcTmAbs x ty1 tm) = do
  kind <- tcType ty1 -- GEORGE: Should have kind star
  unless (kind == KStar) $
    throwError "tcTerm: Kind mismatch (FcTmAbs)"
  (fc_tm, ty2)  <- extendCtxTmM x ty1 (tcTerm tm)
  return (FcTmAbs x ty1 fc_tm, mkFcArrowTy ty1 ty2)
tcTerm (FcTmVar x) = do
  ty <- lookupTmVarM x
  return (FcTmVar x, ty)
tcTerm (FcTmApp tm1 tm2)  = do
  (fc_tm1, ty1) <- tcTerm tm1
  (fc_tm2, ty2) <- tcTerm tm2
  case isFcArrowTy ty1 of
    Just (ty1a, ty1b) -> alphaEqFcTypes ty1a ty2 >>= \case
      True  -> return (FcTmApp fc_tm1 fc_tm2, ty1b)
      False -> throwErrorM (text "tcTerm" <+> text "FcTmApp" $$ pprPar ty1 $$ pprPar ty2)
    Nothing           -> throwErrorM (text "Wrong function FcType application"
                                      $$ parens (text "ty1=" <+> ppr ty1)
                                      $$ parens (text "ty2=" <+> ppr ty2))

tcTerm (FcTmTyAbs a tm) = do
  tyVarNotInFcCtxM a -- GEORGE: Ensure not already bound
  (fc_tm, ty) <- extendCtxTyM a (kindOf a) (tcTerm tm)
  return (FcTmTyAbs a fc_tm, FcTyAbs a ty)
tcTerm (FcTmTyApp tm ty) = do
  kind <- tcType ty
  tcTerm tm >>= \case
    (fc_tm, FcTyAbs a tm_ty)
      | kindOf a == kind -> return (FcTmTyApp fc_tm ty, substVar a ty tm_ty)
    _other               -> throwError "Malformed type application"

tcTerm (FcTmDataCon dc) = do
  ty <- mkDataConTy <$> lookupDataConTyM dc
  return (FcTmDataCon dc, ty)
tcTerm (FcTmLet x ty tm1 tm2) = do
  tmVarNotInFcCtxM x -- GEORGE: Ensure not already bound
  kind <- tcType ty
  unless (kind == KStar) $
    throwError "tcTerm: Kind mismatch (FcTmLet)"
  (fc_tm1, ty1)  <- extendCtxTmM x ty (tcTerm tm1)
  unless (ty `eqFcTypes` ty1) $ throwError "Let type doesnt match"
  (fc_tm2, ty2)   <- extendCtxTmM x ty (tcTerm tm2)
  return (FcTmLet x ty fc_tm1 fc_tm2, ty2)
tcTerm (FcTmCaseFc scr alts) = do
  (fc_scr, scr_ty) <- tcTerm scr
  (fc_alts, ty)    <- tcAlts scr_ty alts
  return (FcTmCaseFc fc_scr fc_alts, ty)
tcTerm (FcTmCaseTc _ rhs_ty scr alts) = do
  (fc_scr, scr_ty) <- tcTerm scr
  x                <- makeVar
  let qs           = map altToEqn (flatAlts alts)
  dsgr             <- extendCtxTmM x scr_ty (match [x] qs (defaultTerm rhs_ty))
  tcTerm (substVar x fc_scr dsgr)
tcTerm (FcTmERROR s ty) = do
  kind <- tcType ty  -- GEORGE: Should have kind star
  unless (kind == KStar) $
    throwError "tcTerm: Kind mismatch (FcTmERROR)"
  return (FcTmERROR s ty, ty)

-- | Kind check a type
tcType :: FcType -> FcM Kind
tcType (FcTyVar a) = lookupTyVarM a
tcType (FcTyAbs a ty) = do
  tyVarNotInFcCtxM a            -- GEORGE: Ensure not already bound
  k <- extendCtxTyM a (kindOf a) (tcType ty)
  case k of
    KStar  -> return KStar
    _other -> throwError "tcType: Kind mismatch (FcTyAbs)"
tcType (FcTyApp ty1 ty2) = do
  k1 <- tcType ty1
  k2 <- tcType ty2
  case k1 of
    KArr k1a k1b | k1a == k2 -> return k1b
    _otherwise               -> throwError "tcType: Kind mismatch (FcTyApp)"
tcType (FcTyCon tc) = lookupTyConKindM tc

-- | Flatten out any or patterns in the alternatives
flatAlts :: FcAlts 'Tc -> FcAlts 'Tc
flatAlts = notImplemented "flatAlts"
  -- concatMap (\(FcAlt p rhs) -> [FcAlt p' rhs | p' <- flatPat p])

-- | Flatten out any or patterns in the pattern
-- flatPat :: FcPat 'Tc -> [FcPat 'Tc]
-- flatPat (FcOrPat p1 p2)    = flatPat p1 ++ flatPat p2
-- flatPat (FcVarPat x)       = [FcVarPat x]
-- flatPat (FcConPatNs dc ps) = [FcConPatNs dc ps' | ps' <- cart (map flatPat ps)]

-- | Type check a list of case alternatives
tcAlts :: FcType -> [FcAlt 'Fc] -> FcM ([FcAlt 'Fc], FcType)
tcAlts scr_ty alts
  | null alts = throwError "Case alternatives are empty"
  | otherwise = do
      (fc_alts, rhs_tys) <- mapAndUnzipM (tcAlt scr_ty) alts
      ensureIdenticalTypes rhs_tys
      let (ty:_) = rhs_tys
      return (fc_alts, ty)

tcAlt :: FcType -> FcAlt 'Fc -> FcM (FcAlt 'Fc, FcType)
tcAlt scr_ty (FcAltFc (FcConPat dc xs) rhs) = case tyConAppMaybe scr_ty of
  Just (tc, tys) -> do
    tmVarsNotInFcCtxM xs -- GEORGE: Ensure not bound already
    (as, arg_tys, dc_tc) <- lookupDataConTyM dc
    unless (dc_tc == tc) $
      throwErrorM (text "tcAlt" <+> colon <+> text "The type of the scrutinee does not match that of the pattern")
    let ty_subst     = mconcat (zipWithExact (|->) as tys)
    let real_arg_tys = map (substFcTyInTy ty_subst) arg_tys
    (fc_rhs, ty)     <- extendCtxTmsM xs real_arg_tys (tcTerm rhs)
    return (FcAltFc (FcConPat dc xs) fc_rhs, ty)
  Nothing -> throwErrorM (text "destructScrTy" <+> colon <+> text "Not a tycon application")

-- * Pattern desugaring
-- ---------------------------------------------

type PmEqn = ([FcPat 'Tc], FcTerm 'Tc)

-- | Get right hand side of an equation
get_rhs :: PmEqn -> FcTerm 'Tc
get_rhs = snd

-- | Convert an Alt to an equation
altToEqn :: FcAlt 'Tc -> PmEqn
altToEqn _ = notImplemented "altToEqn"
-- altToEqn (FcAltTc p t) = ([p], t)

-- | Create a default term
defaultTerm :: FcType -> FcTerm 'Fc
defaultTerm ty = FcTmERROR "Match Failed" ty

-- | Check if first pattern of equation contains a variable
isVar :: PmEqn -> Bool
isVar (((FcVarPat _):_), _) = True
isVar _                     = False

-- | Group the equations based on if they start with a variable or constructor
groupEqns :: [PmEqn] -> [[PmEqn]]
groupEqns = partition isVar

-- | Generate fresh renamed variable
makeVar :: FcM FcTmVar
makeVar = freshFcTmVar

-- Choose all equations that start with the given data constructor
choose :: FcDataCon -> [PmEqn] -> [PmEqn]
choose c qs = [q | q@(((FcConPatNs c' _):_), _) <- qs, c == c']

-- Get list of all constructors of the same type as the first data constructor in the equation
constructors :: [PmEqn] -> FcM [FcDataCon]
constructors (((FcConPatNs dc _):_, _):_) = do
  ty_con_info <- lookupDataConTyConInfoM dc
  return $ fc_tc_data_cons ty_con_info
constructors _                            = return []

-- Get ariry of data constructor
arity :: FcDataCon -> FcM Int
arity dc = length . fc_dc_arg_tys <$> lookupDataConInfoM dc

getRealArgTys :: FcType -> [FcTyVar] -> [FcType] -> [FcType]
getRealArgTys ty as arg_tys = case tyConAppMaybe ty of
  Just (_, tys) -> let ty_subst = mconcat (zipWithExact (|->) as tys) in
    map (substFcTyInTy ty_subst) arg_tys
  Nothing       -> panic "getRealArgTys: not a type constructor application"

-- | Match equations according to the variable rule
matchVar :: [FcTmVar] -> [PmEqn] -> FcTerm 'Fc -> FcM (FcTerm 'Fc)
matchVar (u:us) qs def = match us [(ps, substVar v (FcTmVar u) rhs) | ((FcVarPat v):ps, rhs) <- qs] def
matchVar []     _  _   = panic "matchVar: empty variables"

-- | Match equations according to the constructor rule
matchCon :: [FcTmVar] -> [PmEqn] -> FcTerm 'Fc -> FcM (FcTerm 'Fc)
matchCon (u:us) qs def = do
  cs     <- constructors qs
  alts   <- mapM (\c -> matchClause c (u:us) (choose c qs) def) cs
  return (FcTmCaseFc (FcTmVar u) alts) -- CLEAN UP: do not drop type information
matchCon []     _  _   = panic "matchCon: empty variables"

-- | Match an alternative clause
matchClause :: FcDataCon -> [FcTmVar] -> [PmEqn] -> FcTerm 'Fc -> FcM (FcAlt 'Fc)
matchClause dc (u:us) qs def = do
  exp_ty       <- lookupTmVarM u
  k            <- arity dc
  us'          <- replicateM k makeVar
  (as, tys, _) <- lookupDataConTyM dc
  let real_tys = getRealArgTys exp_ty as tys
  let mtch     = match (us' ++ us) [(ps' ++ ps, rhs) | ((FcConPatNs _ ps'):ps, rhs) <- qs] def
  fc_rhs       <- extendCtxTmsM us' real_tys mtch -- REINERT: george doesn't like this binding variables.
  return (FcAltFc (FcConPat dc us') fc_rhs)
matchClause _   []    _  _   = panic "matchClause: empty variables"

-- | Match a list of equations according to variable or constructor rule
matchVarCon :: [FcTmVar] -> [PmEqn] -> FcTerm 'Fc -> FcM (FcTerm 'Fc)
matchVarCon us (q@(((FcConPatNs _ _):_), _):qs) def = matchCon us (q:qs) def
matchVarCon us (q@(((FcVarPat   _  ):_), _):qs) def = matchVar us (q:qs) def
matchVarCon _  _                                _   = panic "matchVarCon: invalid equations"

-- | Main match function
match :: [FcTmVar] -> [PmEqn] -> FcTerm 'Fc -> FcM (FcTerm 'Fc)
match (u:us) qs     def = foldrM (matchVarCon (u:us)) def (groupEqns qs)
match []     (q:_)  _   = fst <$> tcTerm (get_rhs q)
match []     []     def = return def

-- | Ensure that all types are syntactically the same
ensureIdenticalTypes :: [FcType] -> FcM ()
ensureIdenticalTypes types = unless (go types) $ throwError "Type mismatch in case rhs"
  where
    go :: [FcType] -> Bool
    go []       = True
    go (ty:tys) = all (eqFcTypes ty) tys

-- * Invoke the complete System F type checker
-- ----------------------------------------------------------------------------

-- GEORGE: Refine the type and also print more stuff out

fcTypeCheck :: (AssocList FcTyCon FcTyConInfo, AssocList FcDataCon FcDataConInfo) -> UniqueSupply -> FcProgram a
            -> (Either String (((FcProgram 'Fc, FcType), UniqueSupply), FcGblEnv), Trace)
fcTypeCheck (tc_env, dc_env) us pgm = runWriter
                                    $ runExceptT
                                    $ flip runStateT  fc_init_gbl_env
                                    $ flip runReaderT fc_init_ctx
                                    $ flip runUniqueSupplyT us
                                    $ tcFcProgram pgm
  where
    fc_init_ctx     = mempty
    fc_init_gbl_env = FcGblEnv tc_env dc_env
