import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Types
import Gen (serProg)

main = defaultMain allTests

allTests = testGroup "Tests"
  [ generatorTests, evalTests ]

generatorTests = localOption (SmallCheckDepth 7) $ testGroup "Generation"
  [ testProperty "Programs have correct size" $
      \n -> changeDepth (const n) $
        over serProg $ \prog -> progSize prog == n
  , testProperty "Programs are valid" $
      \n -> changeDepth (const n) $
        over serProg isValid
  ]

evalTests = testGroup "Evaluation" $
  [ testCase "Main arg" $
      ev MainArg 0x42 @?= 0x42
  , testCase "If0 true" $
      ev_ (If Zero One two) @?= 1
  , testCase "If0 false" $
      ev_ (If two One two) @?= 2
  , testCase "Not" $
      ev (Not MainArg)   0x00FF0FF000000000 @?= 0xFF00F00FFFFFFFFF
  , testCase "Shl1" $
      ev (Shl1 MainArg)  0xFFFFFFFFFFFFFFFF @?= 0xFFFFFFFFFFFFFFFE
  , testCase "Shr1" $
      ev (Shr1 MainArg)  0xFFFFFFFFFFFFFFFF @?= 0x7FFFFFFFFFFFFFFF
  , testCase "Shr4" $
      ev (Shr4 MainArg)  0xFFFFFFFFFFFFFFFF @?= 0x0FFFFFFFFFFFFFFF
  , testCase "Shr16" $
      ev (Shr16 MainArg) 0xFFFFFFFFFFFFFFFF @?= 0x0000FFFFFFFFFFFF
  , testCase "Plus" $
      ev (Plus MainArg MainArg) 0xFFFFFFFFFFFFFFFF @?= 0xFFFFFFFFFFFFFFFE
  , testCase "Fold (commutative)" $
      ev (Fold MainArg Zero (Plus Fold1Arg Fold2Arg)) 0x110318FF1609AA98 @?= 652
  , testCase "Fold (non-commutative)" $
      ev (Fold MainArg Zero (Plus (Shl1 Fold1Arg) Fold2Arg)) 0x110318FF1609AA98 @?= 1304
  ]

  where
    ev prog x = eval x undefined undefined prog
    ev_ prog = ev prog undefined
    two = Plus One One

progSize :: Exp -> Int
progSize e = expSize e + 1

expSize :: Exp -> Int
expSize Zero = 1
expSize One = 1
expSize MainArg = 1
expSize Fold1Arg = 1
expSize Fold2Arg = 1
expSize (If a b c) = 1 + expSize a + expSize b + expSize c
expSize (Fold a b c) = 2 + expSize a + expSize b + expSize c
expSize (Not a) = 1 + expSize a
expSize (Shl1 a) = 1 + expSize a
expSize (Shr1 a) = 1 + expSize a
expSize (Shr4 a) = 1 + expSize a
expSize (Shr16 a) = 1 + expSize a
expSize (And a b) = 1 + expSize a + expSize b
expSize (Or a b) = 1 + expSize a + expSize b
expSize (Xor a b) = 1 + expSize a + expSize b
expSize (Plus a b) = 1 + expSize a + expSize b

isValid :: Exp -> Bool
isValid e = noBrokenRefs e && (numFolds e <= 1)
  where
    numFolds Zero = 0
    numFolds One = 0
    numFolds MainArg = 0
    numFolds Fold1Arg = 0
    numFolds Fold2Arg = 0
    numFolds (If a b c) = numFolds a + numFolds b + numFolds c
    numFolds (Fold a b c) = 1 + numFolds a + numFolds b + numFolds c
    numFolds (Not a) = numFolds a
    numFolds (Shl1 a) = numFolds a
    numFolds (Shr1 a) = numFolds a
    numFolds (Shr4 a) = numFolds a
    numFolds (Shr16 a) = numFolds a
    numFolds (And a b) = numFolds a + numFolds b
    numFolds (Or a b) = numFolds a + numFolds b
    numFolds (Xor a b) = numFolds a + numFolds b
    numFolds (Plus a b) = numFolds a + numFolds b

    -- Checks that there are no references to Fold1Arg or Fold2Arg outside a Fold.
    noBrokenRefs Zero = True
    noBrokenRefs One = True
    noBrokenRefs MainArg = True
    noBrokenRefs Fold1Arg = False
    noBrokenRefs Fold2Arg = False
    noBrokenRefs (If a b c) = noBrokenRefs a && noBrokenRefs b && noBrokenRefs c
    noBrokenRefs (Fold a b c) = True
    noBrokenRefs (Not a) = noBrokenRefs a
    noBrokenRefs (Shl1 a) = noBrokenRefs a
    noBrokenRefs (Shr1 a) = noBrokenRefs a
    noBrokenRefs (Shr4 a) = noBrokenRefs a
    noBrokenRefs (Shr16 a) = noBrokenRefs a
    noBrokenRefs (And a b) = noBrokenRefs a && noBrokenRefs b
    noBrokenRefs (Or a b) = noBrokenRefs a && noBrokenRefs b
    noBrokenRefs (Xor a b) = noBrokenRefs a && noBrokenRefs b
    noBrokenRefs (Plus a b) = noBrokenRefs a && noBrokenRefs b
