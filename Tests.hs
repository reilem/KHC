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
performTests (test:tests) = do
  runWithExpected $ parseTest test
  performTests tests

runWithExpected :: (FilePath, String) -> IO ()
runWithExpected (path, expected) = do
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
putFailure expect actual = putResult ((colorDoc red $ text "FAIL")
                            <+> lparen
                            <+> (text "Expected") <+> colon <+> (text expect)
                            <+> comma
                            <+> (text "Actual") <+> colon <+> (text actual)
                            <+> rparen)

putSuccess :: FilePath -> IO ()
putSuccess = putResult (colorDoc green $ text "SUCCESS")

putResult :: Doc -> FilePath -> IO ()
putResult result path = putStrLn $ renderWithColor
  ((colorDoc yellow $ text "TEST")
  <+> colon
  <+> (text path)
  <+> rarrow
  <+> result)

parseTest :: String -> (String, String)
parseTest []           = error "No colon found in test"
parseTest (':':expect) = ([], expect)
parseTest (c:cs)       = let (p, e) = parseTest cs in (c:p, e)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith (a:as) (b:bs)
  | a == b           = startsWith as bs
  | otherwise        = False
startsWith []     _  = True
startsWith (_:_)  [] = False
