ALL=0
FAIL=0
SUCCESS=0

while getopts afs opt
do  case "$opt" in
    a) ALL=1;;
    f) FAIL=1;;
    s) SUCCESS=1;;
    esac
done

if [ "$ALL" == "0" ] && [ "$FAIL" == "0" ] && [ "$SUCCESS" == "0" ]
then
  ALL=1
fi

if [ "$ALL" == "1" ] || [ "$SUCCESS" == "1" ]
then
  echo "GUARDS SIMPLE TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/SimpleTest.hs"
  echo "GUARDS SIMPLE WITH OR TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/SimpleWithOrTest.hs"
  echo "GUARDS SIMPLE FLIPPED TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/SimpleFlippedTest.hs"
  echo "NESTED CASE OF GUARDS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/NestedCaseOfWithGuardsTest.hs"
  echo "MULTIPLE GUARDS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/MultipleGuardsTest.hs"
  echo "COMPLEX TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/ComplexTest.hs"
fi

if [ "$ALL" == "1" ] || [ "$FAIL" == "1" ]
then
  echo "PARSE FAIL NESTED UP GUARDS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailNestedUpGuardsTest.hs"
  echo "PARSE FAIL NESTED DOWN GUARDS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailNestedDownGuardsTest.hs"
  echo "PARSE FAIL MIX GUARDED AND EQUATION TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailMixGuardedAndEquation.hs"
  echo "PARSE FAIL MIX EQUATION AND GUARDED TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailMixEquationAndGuarded.hs"
  echo "PARSE FAIL GUARDS WITHOUT BAR TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailGuardsWithoutBarTest.hs"
  echo "PARSE FAIL BAR WITHOUT GUARDS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailBarWithoutGuardTest.hs"

  echo "RENAME FAIL UNBOUND IN GUARD TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailUnboundInGuardTest.hs"
  echo "RENAME FAIL UNBOUND IN RHS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailUnboundInRhsTest.hs"

  echo "TYPECHECK FAIL INCONSISTENT GUARDS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailInconsistentGuardsTest.hs"
  echo "TYPECHECK FAIL INCONSISTENT RHS TEST"
  echo "==================="
  stack --silent runghc Main.hs "Tests/Guards/FailInconsistentRhsTest.hs"
fi
