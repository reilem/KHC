{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds  #-}

module Backend.FcLinker (fcLink) where

import Backend.FcTypes

-- | Convert a program to a simple expression (local let-bindings).
-- Essentially: all value bindings are converted into let-bindings, data
-- declarations are ignored.
collapseProgram :: FcProgram 'Fc -> FcTerm 'Fc
collapseProgram = \case
  FcPgmTerm e                        ->  e
  FcPgmDataDecl _ p                  -> collapseProgram p
  FcPgmValDecl (FcValBind x ty e1) p -> FcTmLet x ty e1 $ collapseProgram p


-- | Grounds the given term by looking for top-level type abstractions, and
-- then applying it on as many unit values as needed. This is only patched
-- grounding, and so no internal type abstractions are grounded. For example:
--
-- > /\ a. let f = <expr1> in <expr2>
--
-- is grounded to:
--
-- > (/\ a. let f = <expr1> in <expr2>) Unit
--
-- But @(let f = <expr1> in /\a. <expr2>)@ is left unchanged. We do this
-- because we know the elaboration and desugaring phases only produce top level
-- type abstractions. So this method will be sufficient in all cases.
-- TODO: In the future, this function should be type-directed: the type should
-- determine whether we should create an application to unit or not.
groundTerm :: FcTerm 'Fc -> FcTerm 'Fc
groundTerm (FcTmTyAbs ty t1) = FcTmTyApp (FcTmTyAbs ty (groundTerm t1)) fcUnitTy
groundTerm t                 = t

-- | Collapses the program and grounds the resulting term
fcLink :: FcProgram 'Fc -> FcTerm 'Fc
fcLink = groundTerm . collapseProgram
