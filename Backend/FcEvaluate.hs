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

import Control.Monad.State

-- * Evaluation Monad
-- ----------------------------------------------------------------------------

type EvM = UniqueSupplyT (State Int)

-- * Operational Semantics
-- ----------------------------------------------------------------------------

-- | Evaluate an expression (small-step).
smallStep :: FcTerm 'Fc -> EvM (Maybe (FcTerm 'Fc))
-- Values (see why we need or-patterns :P)
smallStep (FcTmAbs {})     = pure Nothing
smallStep (FcTmTyAbs {})   = pure Nothing
smallStep (FcTmDataCon {}) = pure Nothing

-- Normal forms (but not values)
smallStep (FcTmVar {})   = pure Nothing
smallStep (FcTmERROR {}) = pure Nothing

-- Error propagation
smallStep (FcTmApp    e1@(FcTmERROR {}) _) = increment >> pure (Just e1)
smallStep (FcTmTyApp  e1@(FcTmERROR {}) _) = increment >> pure (Just e1)
smallStep (FcTmCaseFc e1@(FcTmERROR {}) _) = increment >> pure (Just e1)

-- Useful cases (substitution)
smallStep (FcTmApp (FcTmAbs x _ body) e2) = Just <$> do
  newe <- freshenLclBndrs e2
  increment
  pure $ substVar x newe body
smallStep (FcTmTyApp (FcTmTyAbs a body) ty) = Just <$> do
  newty <- freshenLclBndrs ty
  increment
  pure $ substVar a newty body
smallStep (FcTmLet x ty e1 e2) = do
  newe1 <- freshenLclBndrs $ FcTmLet x ty e1 e1
  increment
  pure <$> Just $ substVar x newe1 e2
smallStep (FcTmCaseFc scr alts)
  | Just (dc, args) <- userDefinedDataConAppMaybe scr = do
    increment
    matchTheAlts dc args alts

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

increment :: EvM ()
increment = get >>= (\x -> put $ x + 1)

-- | Check whether a term is a user-defined data constructor application.
isUserDefinedDataConApp :: FcTerm 'Fc -> Bool
isUserDefinedDataConApp = isJust . userDefinedDataConAppMaybe
{-# INLINE isUserDefinedDataConApp #-}

-- | Check whether a term is a user defined data constructor application
-- and return its parts.
userDefinedDataConAppMaybe :: FcTerm 'Fc -> Maybe (FcDataCon, [FcTerm 'Fc])
userDefinedDataConAppMaybe tm
  | Just (dc, fn) <- go tm = Just (dc, fn [])
  | otherwise              = Nothing
  where
    go (FcTmDataCon dc) = Just (dc, id)
    go (FcTmApp e1 e2)  | Just (dc, args) <- go e1 = Just (dc, args . (e2:))
    go (FcTmTyApp e1 _) = go e1
    go _ = Nothing

-- | Matches the given data constructor to one of the given alternatives, and
-- substitutes the variables in the alternative's right hand side with the
-- data constructor's arguments.
matchTheAlts :: FcDataCon -> [FcTerm 'Fc] -> [FcAlt 'Fc] -> EvM (Maybe (FcTerm 'Fc))
matchTheAlts _dc _args []                           = error "<impossible: pm compiler failed!>"
matchTheAlts  dc  args ((FcAltFc (FcConPat dc' xs) rhs):rest)
  | dc == dc' = if
      | length args /= length xs -> error "<impossible: elab failed!>"
      | otherwise -> do
          sub <- buildFcTmSubst . zipExact xs <$> mapM freshenLclBndrs args
          pure $ Just $ substFcTmInTm sub rhs
  | otherwise = matchTheAlts dc args rest

-- | Perform multi-step evaluation using call-by-name operational semantics. This
-- function repeatedly performs single-step evaluation, until the result is either in
-- Weak Head Normal Form (WHNF), or is an 'FcTmERROR'.
fullStep :: FcTerm 'Fc -> EvM (Either String (FcTerm 'Fc))
fullStep t = smallStep t >>= \case
  -- If result is another term, continue full evaluation
  Just t'  -> fullStep t'
  -- If no result, evaluation is finished: return final term, or an error
  Nothing  -> case t of
    FcTmERROR err _ -> return $ Left err
    res             -> return $ Right res

-- | Fully evaluates a given term.
fullEval :: FcTerm 'Fc -> EvM (Either String (FcTerm 'Fc))
fullEval t = fullStep t >>= \case
  Right (FcTmApp t1 t2) -> fullEval t1 >>= \case
    Right t1' -> fullEval t2 >>= \case
      Right t2' -> fullStep (FcTmApp t1' t2')
      Left err  -> return $ Left err
    Left err  -> return $ Left err
  result -> return $ result

-- | Fully evaluates the given term. Evaluation is run using unique supply
-- to allow for variable freshening during substitution. The state
-- monad is used to count the number of evaluation steps required.
fcEvaluate :: UniqueSupply -> FcTerm 'Fc -> ((Either String (FcTerm 'Fc), UniqueSupply), Int)
fcEvaluate us tm = runState (runUniqueSupplyT (fullEval tm) us) 0
