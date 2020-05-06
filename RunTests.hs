{-# LANGUAGE LambdaCase #-}

module Utils.TestSuite (main) where

import Frontend.HsParser      (hsParse)
import Frontend.HsRenamer     (hsRename)
import Frontend.HsTypeChecker (hsTypeCheck)
import Backend.FcTypeChecker  (fcTypeCheck)
import Backend.FcEvaluate     (fcEvaluate)

import Utils.Unique  (newUniqueSupply)
import Utils.PrettyPrint

import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, span)

main :: IO ()
main = runTests "ConfigTests.txt"

runTests :: FilePath -> IO ()
runTests config = do
  file  <- readFile config
  performTests $ lines file

performTests :: [String] -> IO ()
performTests []           = return ()
performTests (test:tests)
  | Just (path, expected) <- parseTest test = do
    runWithExpected path expected
    performTests tests
  | otherwise                               = performTests tests

runWithExpected :: FilePath -> String -> IO ()
runWithExpected path expected = do
  result <- runSingleTest path
  if testPass expected result then
    putSuccess path
  else
    putFailure expected result path


-- | Run a single testfile to retrieve evaluation result
runSingleTest :: FilePath -> IO String
runSingleTest file = hsParse file >>= \case
  Left err     -> testError "parser" err
  Right ps_pgm -> do
    us0 <- newUniqueSupply 'u'
    case hsRename us0 ps_pgm of
      (Left err,_) -> testError "renamer" err
      (Right (((rn_pgm, _), us1), rn_env), _) ->
        case hsTypeCheck rn_env us1 rn_pgm of
          (Left err,_) -> testError "typechecker" err
          (Right ((((tc_pgm, _, _), envs), us2), _), _) ->
            case fcTypeCheck envs us2 tc_pgm of
              (Left err,_) -> testError "System F typechecker" err
              (Right (((fc_pgm, _), us3), _), _) -> do
                case fcEvaluate us3 fc_pgm of
                  (Left err, _) -> testError "Evaluation error" err
                  (Right res, _) -> return $ render $ ppr res

testError :: String -> String -> IO String
testError phase e
  | label <- text phase <+> text "failure"
  , msg   <- brackets label <+> text e
  = return (render msg)

putFailure :: String -> String -> FilePath -> IO ()
putFailure expect actual path = putStrLn $ renderWithColor $
    (colorDoc yellow $ text "TEST")
    <+> (colorDoc red $ text "FAIL")
    <+> colon
    <+> (text path)
    <+> rarrow
    <+> parens (
      (colorDoc yellow $ text "Expected") <+> colon <+> (text expect)
      <+> comma
      <+> (colorDoc yellow $ text "Actual") <+> colon <+> (text actual))

putSuccess :: FilePath -> IO ()
putSuccess path = putStrLn $ renderWithColor $
  (colorDoc yellow $ text "TEST")
  <+> (colorDoc green $ text "SUCCESS")
  <+> colon
  <+> (text path)

-- | Parse a 'String' of the form "<path-of-text> : <expected-result>".
-- Removes leading and trailing whitespace from all components.
parseTest :: String -> Maybe (String, String)
parseTest input
  | (path,':':expected) <- span (/=':') input
  = Just (trim path, trim expected)
  | otherwise
  = Nothing

-- | Remove leading and trailing whitespace from a 'String'.
-- NOTE: This implementation is a bit inefficient but simple.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | If the expected outcome is failure, only check for prefix match,
-- otherwise check for exact match.
testPass :: String -> String -> Bool
testPass expected actual
  | isInfixOf "failure" expected = isPrefixOf expected actual
  | otherwise                    = expected == actual
