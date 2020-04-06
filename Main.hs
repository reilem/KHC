{-# LANGUAGE LambdaCase #-}

module Main (main, runTest) where

import Frontend.HsParser      (hsParse)
import Frontend.HsRenamer     (hsRename)
import Frontend.HsTypeChecker (hsTypeCheck)
import Backend.FcTypeChecker  (fcTypeCheck)

import Utils.Unique  (newUniqueSupply)
import Utils.PrettyPrint
import Utils.Trace

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  [filename] -> runTest filename
  _other     -> putStrLn "Usage: ghc <filename>"

-- | Run a single testfile
runTest :: FilePath -> IO ()
runTest file = do
  -- Parsing
  hsParse file >>= \case
    Left err     -> throwMainError "parser" err
    Right ps_pgm -> do
      -- Renaming
      us0 <- newUniqueSupply 'u'
      case hsRename us0 ps_pgm of
        (Left err,_) -> throwMainError "renamer" err
        (Right (((rn_pgm, _rn_ctx), us1), rn_env), _) ->
          case hsTypeCheck rn_env us1 rn_pgm of
            (Left err,_) -> throwMainError "typechecker" err
            (Right ((((tc_pgm, tc_ty, theory), envs), us2), _tc_env), _hsTrace) ->
                  case fcTypeCheck envs us2 tc_pgm of
                    (Left err, _trace) -> do
                      putStrLn $ render (dumpTrace _hsTrace)
                      putStrLn $ render (dumpTrace _trace)
                      throwMainError "System F typechecker" err
                    (Right (((fc_pgm, fc_ty), _us3), _fc_env), _trace) -> do
                      putStrLn $ render (dumpTrace _hsTrace)
                      putStrLn $ render (dumpTrace _trace)
                      putStrLn "---------------------------- Type Checked Program ---------------------------"
                      putStrLn $ renderWithColor $ ppr tc_pgm
                      putStrLn "---------------------------- Elaborated Program ---------------------------"
                      putStrLn $ renderWithColor $ ppr fc_pgm
                      putStrLn "------------------------------- Program Type ------------------------------"
                      putStrLn $ renderWithColor $ ppr tc_ty
                      putStrLn "------------------------------ Program Theory -----------------------------"
                      putStrLn $ renderWithColor $ ppr theory
                      putStrLn "-------------------------- System F Program Type --------------------------"
                      putStrLn $ renderWithColor $ ppr fc_ty
  where
    throwMainError phase e
      | label <- colorDoc red (text phase <+> text "failure")
      , msg   <- brackets label <+> colorDoc red (text e)
      = putStrLn (renderWithColor msg)
