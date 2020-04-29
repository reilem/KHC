{-# LANGUAGE LambdaCase #-}

module Utils.TestSuite (main) where

import Frontend.HsParser      (hsParse)
import Frontend.HsRenamer     (hsRename)
import Frontend.HsTypeChecker (hsTypeCheck)
import Backend.FcTypeChecker  (fcTypeCheck)
import Backend.FcEvaluate     (fcEvaluate)

import Utils.Unique  (newUniqueSupply)
import Utils.PrettyPrint

main :: IO ()
main = runTests "TestConfig.txt"

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
  if startsWith expected result then
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

parseTest :: String -> Maybe (String, String)
parseTest []                    = Nothing
parseTest (':':expect)          = Just ([], expect)
parseTest (c:cs)
  | Just (p, e) <- parseTest cs = Just (c:p, e)
  | otherwise                   = Nothing

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith (a:as) (b:bs)
  | a == b           = startsWith as bs
  | otherwise        = False
startsWith []     _  = True
startsWith (_:_)  [] = False
