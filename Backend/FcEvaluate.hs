{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Backend.FcEvaluate where

import Backend.FcTypes
import Utils.Substitution
import Utils.Unique
import Utils.Utils
import Data.Maybe

-- * Evaluation Monad
-- ----------------------------------------------------------------------------

type EvM = UniqueSupplyM

-- * Operational Semantics
-- ----------------------------------------------------------------------------

-- | Evaluate an expression (small-step).
smallStep :: FcTerm 'Fc -> EvM (Maybe (FcTerm 'Fc))
-- Values (see why we need or-patterns :P)
smallStep (FcTmAbs {})     = pure Nothing
smallStep (FcTmTyAbs {})   = pure Nothing
smallStep (FcTmUnit {})    = pure Nothing
smallStep (FcTmDataCon {}) = pure Nothing

-- Normal forms (but not values)
smallStep (FcTmVar {})   = pure Nothing
smallStep (FcTmERROR {}) = pure Nothing

-- Error propagation
smallStep (FcTmApp    e1@(FcTmERROR {}) _) = pure $ Just e1
smallStep (FcTmTyApp  e1@(FcTmERROR {}) _) = pure $ Just e1
smallStep (FcTmCaseFc e1@(FcTmERROR {}) _) = pure $ Just e1

-- Useful cases (substitution)
smallStep (FcTmApp (FcTmAbs x _ body) e2) = Just <$> do
  newe <- freshenLclBndrs e2
  pure $ substVar x newe body
smallStep (FcTmTyApp (FcTmTyAbs a body) ty) = Just <$> do
  newty <- freshenLclBndrs ty
  pure $ substVar a newty body
smallStep (FcTmLet x ty e1 e2) = do
  newe1 <- freshenLclBndrs $ FcTmLet x ty e1 e1
  pure <$> Just $ substVar x newe1 e2
smallStep (FcTmCaseFc scr alts)
  | FcTmUnit <- scr
  = matchTheUnit alts
  | Just (dc, args) <- userDefinedDataConAppMaybe scr
  = matchTheAlts dc args alts

-- Congruences
smallStep (FcTmApp e1 e2) = smallStep e1 >>= \case
  Just e1' -> pure $ Just $ FcTmApp e1' e2
  Nothing  -> pure Nothing
smallStep (FcTmTyApp e1 ty) = smallStep e1 >>= \case
  Just e1' -> pure $ Just $ FcTmTyApp e1' ty
  Nothing  -> pure Nothing
smallStep (FcTmCaseFc scr alts) = smallStep scr >>= \case
  Just scr' -> pure $ Just $ FcTmCaseFc scr' alts
  Nothing   -> pure $ Nothing

-- | Check whether a term is a user defined data constructor application.
isUserDefinedDataConApp :: FcTerm 'Fc -> Bool
isUserDefinedDataConApp = isJust . userDefinedDataConAppMaybe
{-# INLINE isUserDefinedDataConApp #-}

-- | Check whether a term is a user defined data constructor application
-- | and return its parts.
userDefinedDataConAppMaybe :: FcTerm 'Fc -> Maybe (FcDataCon, [FcTerm 'Fc])
userDefinedDataConAppMaybe tm
  | Just (dc, fn) <- go tm = Just (dc, fn [])
  | otherwise              = Nothing
  where
    -- TODO: ADD UNIT
    go (FcTmDataCon dc) = Just (dc, id)
    go (FcTmApp e1 e2)  | Just (dc, args) <- go e1 = Just (dc, args . (e2:))
    go (FcTmTyApp e1 _) = go e1
    go _ = Nothing

-- | TODO: remove
matchTheUnit :: [FcAlt 'Fc] -> EvM (Maybe (FcTerm 'Fc))
matchTheUnit []                              = error "<unit impossible: pm compiler failed!>"
matchTheUnit ((FcAltFc FcUnitPat     rhs):_) = pure $ Just rhs
matchTheUnit ((FcAltFc (FcConPat {}) _  ):_) = error "<unit impossible: elab failed!>"

-- | Matches the given data constructor to one of the given alternatives, and
-- | substitutes the variables in the alternative's right hand side with the
-- | data constructor's arguements.
matchTheAlts :: FcDataCon -> [FcTerm 'Fc] -> [FcAlt 'Fc] -> EvM (Maybe (FcTerm 'Fc))
matchTheAlts _dc _args []                           = error "<impossible: pm compiler failed!>"
matchTheAlts _dc _args ((FcAltFc FcUnitPat _rhs):_) = error "<impossible: elab failed!>"
matchTheAlts  dc  args ((FcAltFc (FcConPat dc' xs) rhs):rest)
  | dc == dc' = if
      | length args /= length xs -> error "<impossible: elab failed!>"
      | otherwise -> do
          sub <- buildFcTmSubst . zipExact xs <$> mapM freshenLclBndrs args
          pure $ Just $ substFcTmInTm sub rhs
  | otherwise = matchTheAlts dc args rest

-- | Perform a multi-step evaluation using lazy operational semantics. This
-- | function performs single-step evaluations until the result of this single
-- | step evaluation indicates that the given term is fully evaluated. If the
-- | term is an error, then the error's string is returned, otherwise the
-- | full term is returned.
fullStep :: FcTerm 'Fc -> EvM (Either String (FcTerm 'Fc))
fullStep t = smallStep t >>= \case
  Just t'  -> fullStep t'
  Nothing  -> case t of
    FcTmERROR err _ -> return $ Left err
    res             -> return $ Right res

-- | Convert a program to a simple expression (local let-bindings).
collapseProgram :: FcProgram 'Fc -> FcTerm 'Fc
collapseProgram = \case
  FcPgmTerm e                        ->  e
  FcPgmDataDecl _ p                  -> collapseProgram p
  FcPgmValDecl (FcValBind x ty e1) p -> FcTmLet x ty e1 $ collapseProgram p

-- | Grounds the given term by wrapping all top level type abstractions in a
-- | type application on unit. This is only patched grounding, and so no
-- | internal type abstractions are grounded. For example:
-- |   /\ a. let f = <expr1> in <expr2>
-- | is grounded to:
-- |   (/\ a. let f = <expr1> in <expr2>) Unit
-- |
-- | But (let f = <expr1> in /\a. <expr2>) is left unchanged. We do this
-- | because we know the elaboration & desugaring phases only produce top level
-- | type abstractions. So this method will be sufficient in all cases.
groundTerm :: FcTerm 'Fc -> FcTerm 'Fc
groundTerm (FcTmTyAbs ty t1) = FcTmTyApp (FcTmTyAbs ty (groundTerm t1)) FcTyUnit
groundTerm t                 = t

-- | Fully evaluates given term using lazy semantics. However if the final
-- | result is a term application, then the right term in the application may
-- | not be fully evaluated. In order to produce a clear, readable output we
-- | choose to evaluate all branches recursively before returning the result.
fullEval :: FcTerm 'Fc -> EvM (Either String (FcTerm 'Fc))
fullEval t = fullStep t >>= \case
  Right (FcTmApp t1 t2) -> fullEval t2 >>= \case
    Right t2' -> fullStep (FcTmApp t1 t2')
    Left err  -> return $ Left err
  result -> return $ result

-- | Evaluates the given program:
-- | 1. Collapse the program to produce a single term that can be evaluated
-- | 2. Ground all top level type abstractions using Unit type,
-- |    otherwise they will prevent meaningful evaluation.
-- | 3. Fully evaluate the grounded term. Evaluation is run using unique supply
-- |    to allow for variable freshening during substitution.
fcEvaluate :: UniqueSupply -> FcProgram 'Fc -> (Either String (FcTerm 'Fc), UniqueSupply)
fcEvaluate us pgm =
  let pgmTm = collapseProgram pgm in
  let gtm   = groundTerm pgmTm in
  runUniqueSupplyM (fullEval gtm) us
