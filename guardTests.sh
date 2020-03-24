echo "GUARDS SIMPLE TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/SimpleTest.hs"
echo "GUARDS SIMPLE FLIPPED TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/SimpleFlippedTest.hs"
echo "NESTED GUARDS TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/NestedGuardsTest.hs"
echo "NESTED CASE OF GUARDS TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/NestedCaseOfWithGuardsTest.hs"
echo "MULTIPLE GUARDS TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/MultipleGuardsTest.hs"

echo "FAIL MIX GUARDED AND EQUATION TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/FailMixGuardedAndEquation.hs"
echo "FAIL MIX EQUATION AND GUARDED TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/FailMixEquationAndGuarded.hs"
echo "FAIL GUARDS WITHOUT BAR TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/FailGuardsWithoutBarTest.hs"
echo "FAIL BAR WITHOUT GUARDS TEST"
echo "==================="
stack --silent runghc Main.hs "Tests/Guards/FailBarWithoutGuardTest.hs"
