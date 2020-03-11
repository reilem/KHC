{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.FcTypes where

import Utils.Unique
import Utils.Var
import Utils.Kind
import Utils.PrettyPrint
import Utils.Annotated
import Utils.FreeVars

import Data.Maybe (isJust)
import Data.Function (on)
import Data.List ((\\))

-- * Arrow Type Constructor
-- ----------------------------------------------------------------------------

mkFcArrowTy :: FcType -> FcType -> FcType
mkFcArrowTy ty1 ty2 = FcTyApp (FcTyApp (FcTyCon fcArrowTyCon) ty1) ty2

fcArrowTyCon :: FcTyCon
fcArrowTyCon = FcTC (mkName (mkSym "(->)") arrowTyConUnique)

isFcArrowTy :: FcType -> Maybe (FcType, FcType)
isFcArrowTy (FcTyApp (FcTyApp (FcTyCon tc) ty1) ty2)
  | tc == fcArrowTyCon  = Just (ty1, ty2)
isFcArrowTy _other_type = Nothing

-- * Types
-- ----------------------------------------------------------------------------
data FcType = FcTyVar FcTyVar        -- ^ Type variable
            | FcTyAbs FcTyVar FcType -- ^ Type abstraction
            | FcTyApp FcType  FcType -- ^ Type application
            | FcTyCon FcTyCon        -- ^ Type constructor

-- | Syntactic equality on System F types
eqFcTypes :: FcType -> FcType -> Bool
eqFcTypes (FcTyVar v1)    (FcTyVar v2)    = v1 == v2
eqFcTypes (FcTyAbs v1 t1) (FcTyAbs v2 t2) = (v1 == v2)      && eqFcTypes t1 t2
eqFcTypes (FcTyApp t1 t2) (FcTyApp t3 t4) = eqFcTypes t1 t3 && eqFcTypes t2 t4
eqFcTypes (FcTyCon tc1)   (FcTyCon tc2)   = tc1 == tc2

eqFcTypes (FcTyVar {}) _ = False
eqFcTypes (FcTyAbs {}) _ = False
eqFcTypes (FcTyApp {}) _ = False
eqFcTypes (FcTyCon {}) _ = False

-- | Type Constructors
newtype FcTyCon = FcTC { unFcTC :: Name }

instance Eq FcTyCon where
  (==) = (==) `on` unFcTC

instance Ord FcTyCon where
  compare = compare `on` unFcTC

instance Symable FcTyCon where
  symOf = symOf . unFcTC

instance Named FcTyCon where
  nameOf = unFcTC

instance Uniquable FcTyCon where
  getUnique = getUnique . unFcTC

data FcTyConInfo
  = FcTCInfo { fc_tc_ty_con    :: FcTyCon       -- ^ The type constructor name
             , fc_tc_type_args :: [FcTyVar]     -- ^ Universal types
             , fc_tc_data_cons :: [FcDataCon] } -- ^ Child data constructors

-- | Pretty print type constructor info
instance PrettyPrint FcTyConInfo where
  ppr (FcTCInfo tc type_args dcs)
    = braces $ vcat $ punctuate comma
    $ [ text "fc_tc_ty_con"    <+> colon <+> ppr tc
      , text "fc_tc_type_args" <+> colon <+> ppr type_args
      , text "fc_tc_data_cons" <+> colon <+> ppr dcs
      ]

  needsParens _ = False

-- | Data Constructors
newtype FcDataCon = FcDC { unFcDC :: Name }
  deriving (Eq, Ord)

instance Symable FcDataCon where
  symOf = symOf . unFcDC

instance Named FcDataCon where
  nameOf = unFcDC

instance Uniquable FcDataCon where
  getUnique = getUnique . unFcDC

data FcDataConInfo
  = FcDCInfo { fc_dc_data_con :: FcDataCon  -- ^ The data constructor name
             , fc_dc_univ     :: [FcTyVar]  -- ^ Universal type variables
             , fc_dc_parent   :: FcTyCon    -- ^ Parent type constructor
             , fc_dc_arg_tys  :: [FcType] } -- ^ Argument types

-- | Pretty print data constructor info
instance PrettyPrint FcDataConInfo where
  ppr (FcDCInfo dc univ tc arg_tys)
    = braces $ vcat $ punctuate comma
    $ [ text "fc_dc_data_con" <+> colon <+> ppr dc
      , text "fc_dc_univ"     <+> colon <+> ppr univ
      , text "fc_dc_parent"   <+> colon <+> ppr tc
      , text "fc_dc_arg_tys"  <+> colon <+> ppr arg_tys
      ]
  needsParens _ = False

-- -- | Take the type apart the hindley milner way
-- destructFcTypeHM :: FcType -> ([FcTyVar], [FcType], FcType)
-- destructFcTypeHM (FcTyArr ty1 ty2) = (as, ty1:lhs, rhs)
--   where (as, lhs, rhs) = destructFcTypeHM ty2
-- destructFcTypeHM (FcTyAbs a ty) = (a:as, lhs, rhs)
--   where (as, lhs, rhs) = destructFcTypeHM ty
-- destructFcTypeHM ty@(FcTyVar  {}) = ([], [], ty)
-- destructFcTypeHM ty@(FcTyApp  {}) = ([], [], ty)
-- destructFcTypeHM ty@(FcTyCon  {}) = ([], [], ty)

constructFcTypeHM :: ([FcTyVar], [FcType], FcType) -> FcType
constructFcTypeHM (as, tys, ty) = fcTyAbs as (fcTyArr tys ty)

-- | Take apart a type constructor application
tyConAppMaybe :: FcType -> Maybe (FcTyCon, [FcType])
tyConAppMaybe ty = go ty []
  where
    go :: FcType -> [FcType] -> Maybe (FcTyCon, [FcType])
    go (FcTyApp ty1 ty2)  tys = go ty1 (ty2:tys)
    go (FcTyCon tc)       tys = Just (tc, tys)
    go _other_ty         _tys = Nothing

-- * Some smart constructors (uncurried variants)
-- ----------------------------------------------------------------------------

-- | Uncurried version of data constructor FcTyAbs
fcTyAbs :: [FcTyVar] -> FcType -> FcType
fcTyAbs vars ty = foldr FcTyAbs ty vars

-- | Uncurried version of data constructor FcTyArr
fcTyArr :: [FcType] -> FcType -> FcType
fcTyArr tys ty = foldr mkFcArrowTy ty tys

-- | Uncurried version of data constructor FcTyApp
fcTyApp :: FcType -> [FcType] -> FcType
fcTyApp ty tys = foldl FcTyApp ty tys

-- | Apply a type constructor to a bunch of types
fcTyConApp :: FcTyCon -> [FcType] -> FcType
fcTyConApp tc tys = fcTyApp (FcTyCon tc) tys

-- * Terms
-- ----------------------------------------------------------------------------

data Phase = Tc | Fc

data FcTerm (a :: Phase) where
  FcTmAbs       :: FcTmVar -> FcType -> FcTerm a -> FcTerm a             -- ^ Term abstraction: lambda x : ty . tm
  FcTmVar       :: FcTmVar -> FcTerm a                                   -- ^ Term variable
  FcTmApp       :: FcTerm a -> FcTerm a -> FcTerm a                      -- ^ Term application
  FcTmTyAbs     :: FcTyVar -> FcTerm a -> FcTerm a                       -- ^ Type abstraction: Lambda a . tm
  FcTmTyApp     :: FcTerm a -> FcType -> FcTerm a                        -- ^ Type application
  FcTmDataCon   :: FcDataCon -> FcTerm a                                 -- ^ Data constructor
  FcTmLet       :: FcTmVar -> FcType -> FcTerm a -> FcTerm a -> FcTerm a -- ^ Let binding: let x : ty = tm in tm
  FcTmCaseFc    :: FcTerm 'Fc -> [FcAlt 'Fc] -> FcTerm 'Fc               -- ^ Case
  FcTmCaseTc    :: FcType -> FcType -> FcTerm 'Tc -> [FcAlt 'Tc] -> FcTerm 'Tc               -- ^ Nested Case
  FcTmERROR     :: String -> FcType -> FcTerm a                          -- ^ Hard-wired error call
-- GEORGE: You should never need to make terms and patterns instances of Eq. If
-- you do it means that something is probably wrong (the only setting where you
-- need stuff like this is for optimizations).

-- | Patterns
data FcPat (a :: Phase) where
  FcConPat    :: FcDataCon -> [FcTmVar]   -> FcPat 'Fc
  FcConPatNs  :: FcDataCon -> [FcPat 'Tc] -> FcPat 'Tc
  FcVarPat    :: FcTmVar   -> FcPat 'Tc
  FcOrPat     :: FcPat 'Tc -> FcPat 'Tc   -> FcPat 'Tc

-- | Case alternative(s)
data FcAlt (a :: Phase) where
  FcAlt :: FcPat a -> FcTerm a -> FcAlt a

type FcAlts a = [FcAlt a]

-- * Some smart constructors (uncurried variants)
-- ----------------------------------------------------------------------------

-- | Uncurried version of data constructor FcTmAbs
fcTmAbs :: [(FcTmVar, FcType)] -> FcTerm a -> FcTerm a
fcTmAbs binds tm = foldr (uncurry FcTmAbs) tm binds

-- | Uncurried version of data constructor FcTmTyAbs
fcTmTyAbs :: [FcTyVar] -> FcTerm a -> FcTerm a
fcTmTyAbs tvs tm = foldr FcTmTyAbs tm tvs

-- | Uncurried version of data constructor FcTmApp
fcTmApp :: FcTerm a -> [FcTerm a] -> FcTerm a
fcTmApp tm tms = foldl FcTmApp tm tms

-- | Uncurried version of data constructor FcTmTyApp
fcTmTyApp :: FcTerm a -> [FcType] -> FcTerm a
fcTmTyApp tm tys = foldl FcTmTyApp tm tys

-- | Create a data constructor application
fcDataConApp :: FcDataCon -> FcType -> [FcTerm a] -> FcTerm a
fcDataConApp dc ty = fcTmApp (FcTmTyApp (FcTmDataCon dc) ty)

-- | Apply a term to a list of dictionary variables
fcDictApp :: FcTerm a -> [DictVar] -> FcTerm a
fcDictApp tm ds = foldl FcTmApp tm (map FcTmVar ds)

-- * Programs and declarations
-- ----------------------------------------------------------------------------

-- | Data Type Declaration
data FcDataDecl = FcDataDecl { fdata_decl_tc   :: FcTyCon                 -- ^ Type Constructor
                             , fdata_decl_tv   :: [FcTyVar]               -- ^ Universal Type variables
                             , fdata_decl_cons :: [(FcDataCon, [FcType])] -- ^ Data Constructors
                             }

-- | Top-level Value Binding
data FcValBind (a :: Phase) = FcValBind { fval_bind_var :: FcTmVar   -- ^ Variable Name
                                        , fval_bind_ty  :: FcType    -- ^ Variable Type
                                        , fval_bind_tm  :: FcTerm a  -- ^ Variable Value
                                        }

-- | Program
data FcProgram (a :: Phase) = FcPgmDataDecl FcDataDecl    (FcProgram a) -- ^ Data Declaration
                            | FcPgmValDecl  (FcValBind a) (FcProgram a) -- ^ Value Binding
                            | FcPgmTerm     (FcTerm a)                  -- ^ Term

-- * Collecting Free Variables Out Of Objects
-- ------------------------------------------------------------------------------

instance ContainsFreeTyVars FcType FcTyVar where
  ftyvsOf (FcTyVar a)       = [a]
  ftyvsOf (FcTyAbs a ty)    = ftyvsOf ty \\ [a]
  ftyvsOf (FcTyApp ty1 ty2) = ftyvsOf ty1 ++ ftyvsOf ty2
  ftyvsOf (FcTyCon _tc)     = []

instance ContainsFreeTyVars (FcTerm a) FcTyVar where
  ftyvsOf (FcTmAbs _ ty tm)      = ftyvsOf ty ++ ftyvsOf tm
  ftyvsOf (FcTmVar{})            = []
  ftyvsOf (FcTmApp tm1 tm2)      = ftyvsOf tm1 ++ ftyvsOf tm2
  ftyvsOf (FcTmTyAbs a tm)       = ftyvsOf tm \\ [a]
  ftyvsOf (FcTmTyApp tm ty)      = ftyvsOf tm ++ ftyvsOf ty
  ftyvsOf (FcTmDataCon{})        = []
  ftyvsOf (FcTmLet _ ty tm1 tm2) = ftyvsOf ty ++ ftyvsOf tm1 ++ ftyvsOf tm2
  ftyvsOf (FcTmCaseFc tm cs)     = ftyvsOf tm ++ ftyvsOf cs
  ftyvsOf (FcTmCaseTc _ _ tm cs) = ftyvsOf tm ++ ftyvsOf cs
  ftyvsOf (FcTmERROR _err ty)    = ftyvsOf ty

instance ContainsFreeTyVars (FcAlt a) FcTyVar where
  ftyvsOf (FcAlt _pat tm) = ftyvsOf tm

-- * Pretty printing
-- ----------------------------------------------------------------------------

isFcTyApp :: FcType -> Bool
isFcTyApp (FcTyApp {}) = True
isFcTyApp _other       = False

isFcTyAbs :: FcType -> Bool
isFcTyAbs (FcTyAbs {}) = True
isFcTyAbs _other       = False

-- | Pretty print types
instance PrettyPrint FcType where
  ppr ty | Just (ty1, ty2) <- isFcArrowTy ty
         , let d1 = if isJust (isFcArrowTy ty1) || isFcTyAbs ty1
                      then pprPar ty1
                      else ppr ty1
         , let d2 = if isJust (isFcArrowTy ty2) || isFcTyApp ty2
                      then ppr ty2
                      else pprPar ty2
         = d1 <+> arrow <+> d2

  ppr (FcTyVar a)       = ppr a
  ppr (FcTyAbs a ty)    = text "forall" <+> ppr a <> dot <+> ppr ty
  ppr (FcTyApp ty1 ty2)
    | FcTyApp {} <- ty1 = ppr ty1    <+> pprPar ty2
    | otherwise         = pprPar ty1 <+> pprPar ty2
  ppr (FcTyCon tc)      = ppr tc

  needsParens (FcTyApp {}) = True
  needsParens (FcTyAbs {}) = True
  needsParens (FcTyVar {}) = False
  needsParens (FcTyCon {}) = False

-- | Pretty print type constructors
instance PrettyPrint FcTyCon where
  ppr           = ppr . symOf -- Do not show the uniques on the constructors
  needsParens _ = False

-- | Pretty print data constructors
instance PrettyPrint FcDataCon where
  ppr           = ppr . symOf -- Do not show the uniques on the constructors
  needsParens _ = False

-- | Pretty print terms
instance PrettyPrint (FcTerm a) where
  ppr (FcTmAbs x ty tm)    = hang (backslash <> parens (ppr x <+> dcolon <+> ppr ty) <> dot) 2 (ppr tm)
  ppr (FcTmVar x)          = ppr x
  ppr (FcTmApp tm1 tm2)
    | FcTmApp   {} <- tm1  = ppr tm1    <+> pprPar tm2
    | FcTmTyApp {} <- tm1  = ppr tm1    <+> pprPar tm2
    | otherwise            = pprPar tm1 <+> pprPar tm2
  ppr (FcTmTyAbs a tm)     = hang (colorDoc yellow (text "/\\") <> ppr a <> dot) 2 (ppr tm)
  ppr (FcTmTyApp tm ty)    = pprPar tm <+> brackets (ppr ty)
  ppr (FcTmDataCon dc)     = ppr dc
  ppr (FcTmLet x ty tm1 tm2)
    =  (colorDoc yellow (text "let") <+> ppr x <+> ((colon <+> ppr ty) $$ (equals <+> ppr tm1)))
    $$ (colorDoc yellow (text "in" ) <+> ppr tm2)

  ppr (FcTmCaseFc tm cs)     = hang (colorDoc yellow (text "case") <+> ppr tm <+> colorDoc yellow (text "of"))
                                  2 (vcat $ map ppr cs)
  ppr (FcTmCaseTc _ _ tm cs) = hang (colorDoc yellow (text "case") <+> ppr tm <+> colorDoc yellow (text "of"))
                                  2 (vcat $ map ppr cs)
  ppr (FcTmERROR s ty)    = text "ERROR" <+> doubleQuotes (text s) <+> dcolon <+> ppr ty

  needsParens (FcTmApp     {}) = True
  needsParens (FcTmTyApp   {}) = True
  needsParens (FcTmLet     {}) = True
  needsParens (FcTmCaseFc    {}) = True
  needsParens (FcTmCaseTc  {}) = True
  needsParens (FcTmAbs     {}) = True
  needsParens (FcTmVar     {}) = False
  needsParens (FcTmTyAbs   {}) = True
  needsParens (FcTmDataCon {}) = False
  needsParens (FcTmERROR   {}) = True

-- | Pretty print patterns
instance PrettyPrint (FcPat a) where
  ppr (FcConPat   dc xs)          = ppr dc <+> hsep (map pprPar xs)
  ppr (FcConPatNs dc ps)          = ppr dc <+> hsep (map pprPar ps)
  ppr (FcVarPat   x)              = ppr x
  ppr (FcOrPat    p1 p2)          = pprPar p1 <+> text "||" <+> pprPar p2
  needsParens (FcVarPat   _)      = False
  needsParens (FcConPat   _dx xs) = length xs > 0
  needsParens (FcConPatNs _dx ps) = length ps > 0
  needsParens (FcOrPat    _   _ ) = False

-- | Pretty print case alternatives
instance PrettyPrint (FcAlt a) where
  ppr (FcAlt p tm) = ppr p <+> arrow <+> ppr tm
  needsParens _    = True

-- | Pretty print data declarations
instance PrettyPrint FcDataDecl where
  ppr (FcDataDecl tc as dcs) = hsep [colorDoc green (text "data"), ppr tc, hsep (map ppr ann_as), cons]
    where
      ann_as = map (\a -> (a :| kindOf a)) as
      ppr_dc (dc, tys) = hsep (colorDoc yellow (char '|') : ppr dc : map pprPar tys)

      cons = sep $ case dcs of
        []               -> []
        ((dc, tys):rest) -> hsep (equals : ppr dc : map pprPar tys) : map ppr_dc rest

  needsParens _ = False

-- | Pretty print top-level value bindings
instance PrettyPrint (FcValBind a) where
  ppr (FcValBind x ty tm) = hsep [ colorDoc yellow (text "let"), ppr x
                                 , vcat [ colon <+> ppr ty, equals <+> ppr tm ]
                                 ]
  needsParens _ = False

-- | Pretty print programs
instance PrettyPrint (FcProgram a) where
  ppr (FcPgmDataDecl datadecl pgm) = ppr datadecl $$ ppr pgm
  ppr (FcPgmValDecl  valbind  pgm) = ppr valbind  $$ ppr pgm
  ppr (FcPgmTerm tm)               = ppr tm
  needsParens _ = False
