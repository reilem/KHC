echo "SIMPLE PATTERN TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/PatternTest.hs"
echo "\nNESTED PATTERN TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/NestedPatternTest.hs"
echo "\nINCOMPLETE PATTERN TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/IncompletePatternTest.hs"
echo "\nPARENTHESIS TEST"
echo "================"
stack --silent runghc Main.hs "Tests/ParensPatternTest.hs"
echo "\nTOP LEVEL BINDING TEST"
echo "======================"
stack --silent runghc Main.hs "Tests/TopLevelVarBindPatternTest.hs"
echo "\nFAIL ARITY TEST"
echo "================"
stack --silent runghc Main.hs "Tests/FailArityWrongPatternTest.hs"
echo "\nFAIL NON DISTINCT VARIABLES TEST"
echo "================================"
stack --silent runghc Main.hs "Tests/FailNonDistinctPatternTest.hs"
echo "\nFAIL WEIRD PATTERN TEST"
echo "======================="
stack --silent runghc Main.hs "Tests/FailWeirdPatternTest.hs"
