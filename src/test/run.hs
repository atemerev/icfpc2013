{-# LANGUAGE ViewPatterns, ImplicitParams #-}
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

import qualified Data.Set as S
import Data.Generics.Uniplate.Data
import Control.Monad

import Types
import Gen
import RandomBV (bvs)
import Data.Maybe
import Data.Bits
import Debug.Trace

main = defaultMain allTests

allTests = testGroup "Tests"
  [ generatorTests, evalTests ]

mask64 :: Word64
mask64 = 0xFFFFFFFFFFFFFFFF

serProgSC r = generate $ \n -> serProg n r False Nothing
serExpressionSC r = generate $ \n -> serExpression' n r False Nothing

generatorTests = let ?bonus = False in localOption (SmallCheckDepth 8) $ testGroup "Generation"
  [ testProperty "Programs have correct size" $
      \n -> changeDepth (const n) $
        over (serProgSC noRestriction) $ \prog -> progSize (expr prog) == n
  , testProperty "Programs are valid" $
      \n -> changeDepth (const n) $
        over (serProgSC noRestriction) (isValid.expr)
  , localOption (SmallCheckDepth 5) $ testProperty "Operator restrictions" $
      \n ->
      over restrictionSeries $ \ops ->

      S.fromList (filter (checkRestriction ops)
        (list n (serProgSC noRestriction)))
        == S.fromList (list n (serProgSC (restrictionFromList ops)))
  , testProperty "Programs have correct cached value for seed" $
      \n -> changeDepth (const n) $
        over (serProgSC noRestriction) $ \prog -> 
        fromMWord64 (error "no cached value in test") (cached prog) == eval (head bvs) undefined undefined prog
  , testProperty "Left and right zero estimates for programs are correct" $
      \n -> changeDepth (const (n-2)) $
        over (serExpressionSC noRestriction) $ \(e, _, lz, rz, _) ->
          over (generate (const bvs)) $ \v ->
            let res = eval v undefined undefined e
            in  (res .&. complement (mask64 `shiftL` rz) == 0 &&
                 res .&. complement (mask64 `shiftR` lz) == 0)
  , testProperty "Left and right zero estimates for programs are correct using leftRightZeros" $
      \n -> changeDepth (const (n-2)) $
        over (serExpressionSC noRestriction) $ \(e, _, lz, rz, _) ->
          over (generate (const bvs)) $ \v ->
            let res = eval v undefined undefined e
            in  (lz, rz) == leftRightZeros (expr e)
  , localOption (SmallCheckDepth 6) $ testProperty "Left constraints generate all programs that satisfy the costraint" $
      \n -> changeDepth (const n) $ do
        over (generate (\n -> [0, 1, 2, 3, 4, 5, 10, 15, 16, 20, 64])) $ \alz ->
          let satisfyRestriction alz (e,_,_,_,_) = case leftRightZeros (expr e) of (lz,rz) -> lz <= alz
          in (S.fromList (filter (satisfyRestriction alz) (list n (serExpressionSC noRestriction)))
               == S.fromList (list n (serExpressionSC (noRestriction {allowedZeroLeftBits = alz}))))
  , localOption (SmallCheckDepth 6) $ testProperty "Right constraints generate all programs that satisfy the costraint" $
      \n -> changeDepth (const n) $ do
        over (generate (\n -> [0, 1, 2, 3, 4, 5, 10, 15, 16, 20, 64])) $ \arz ->
          let satisfyRestriction arz (e,_,_,_,_) = case leftRightZeros (expr e) of (lz,rz) -> rz <= arz
          in (S.fromList (filter (satisfyRestriction arz) (list n (serExpressionSC noRestriction)))
               == S.fromList (list n (serExpressionSC (noRestriction {allowedZeroRightBits = arz}))))
  ]

evalTests = testGroup "Evaluation" $
  [ testCase "Main arg" $
      ev mainArg 0x42 @?= 0x42
  , testCase "If0 true" $
      ev_ (if0 zero one two) @?= 1
  , testCase "If0 false" $
      ev_ (if0 two one two) @?= 2
  , testCase "Not" $
      ev (not_ mainArg)   0x00FF0FF000000000 @?= 0xFF00F00FFFFFFFFF
  , testCase "Shl1" $
      ev (shl1 mainArg)  0xFFFFFFFFFFFFFFFF @?= 0xFFFFFFFFFFFFFFFE
  , testCase "Shr1" $
      ev (shr1 mainArg)  0xFFFFFFFFFFFFFFFF @?= 0x7FFFFFFFFFFFFFFF
  , testCase "Shr4" $
      ev (shr4 mainArg)  0xFFFFFFFFFFFFFFFF @?= 0x0FFFFFFFFFFFFFFF
  , testCase "Shr16" $
      ev (shr16 mainArg) 0xFFFFFFFFFFFFFFFF @?= 0x0000FFFFFFFFFFFF
  , testCase "Plus" $
      ev (plus mainArg mainArg) 0xFFFFFFFFFFFFFFFF @?= 0xFFFFFFFFFFFFFFFE
  , testCase "Fold (commutative)" $
      ev (fold_ mainArg zero (plus fold1Arg fold2Arg)) 0x110318FF1609AA98 @?= 652
  , testCase "Fold (non-commutative)" $
      ev (fold_ mainArg zero (plus (shl1 fold1Arg) fold2Arg)) 0x110318FF1609AA98 @?= 1304
  ]

  where
    ev prog x = eval x undefined undefined prog
    ev_ prog = ev prog undefined
    two = plus one one

isValid :: Exp -> Bool
isValid e = noBrokenRefs e && (numFolds e <= 1)
  where
    numFoldsC (ExpC _ e) = numFolds e
    numFolds Zero = 0
    numFolds One = 0
    numFolds MainArg = 0
    numFolds Fold1Arg = 0
    numFolds Fold2Arg = 0
    numFolds (If a b c) = numFoldsC a + numFoldsC b + numFoldsC c
    numFolds (Fold a b c) = 1 + numFoldsC a + numFoldsC b + numFoldsC c
    numFolds (Not a) = numFoldsC a
    numFolds (Shl1 a) = numFoldsC a
    numFolds (Shr1 a) = numFoldsC a
    numFolds (Shr4 a) = numFoldsC a
    numFolds (Shr16 a) = numFoldsC a
    numFolds (And a b) = numFoldsC a + numFoldsC b
    numFolds (Or a b) = numFoldsC a + numFoldsC b
    numFolds (Xor a b) = numFoldsC a + numFoldsC b
    numFolds (Plus a b) = numFoldsC a + numFoldsC b

    -- Checks that there are no references to Fold1Arg or Fold2Arg outside a Fold.
    noBrokenRefsC (ExpC _ e) = noBrokenRefs e
    
    noBrokenRefs Zero = True
    noBrokenRefs One = True
    noBrokenRefs MainArg = True
    noBrokenRefs Fold1Arg = False
    noBrokenRefs Fold2Arg = False
    noBrokenRefs (If a b c) = noBrokenRefsC a && noBrokenRefsC b && noBrokenRefsC c
    noBrokenRefs (Fold a b c) = True
    noBrokenRefs (Not a) = noBrokenRefsC a
    noBrokenRefs (Shl1 a) = noBrokenRefsC a
    noBrokenRefs (Shr1 a) = noBrokenRefsC a
    noBrokenRefs (Shr4 a) = noBrokenRefsC a
    noBrokenRefs (Shr16 a) = noBrokenRefsC a
    noBrokenRefs (And a b) = noBrokenRefsC a && noBrokenRefsC b
    noBrokenRefs (Or a b) = noBrokenRefsC a && noBrokenRefsC b
    noBrokenRefs (Xor a b) = noBrokenRefsC a && noBrokenRefsC b
    noBrokenRefs (Plus a b) = noBrokenRefsC a && noBrokenRefsC b

expOps :: Exp -> S.Set OpName
expOps = S.unions . map nodeToOp . universe
  where
    nodeToOp e =
      case e of
        Not {} -> S.singleton Not_op
        Shl1 {} -> S.singleton Shl1_op
        Shr1 {} -> S.singleton Shr1_op
        Shr4 {} -> S.singleton Shr4_op
        Shr16 {} -> S.singleton Shr16_op
        And {} -> S.singleton And_op
        Or {} -> S.singleton Or_op
        Xor {} -> S.singleton Xor_op
        Plus {} -> S.singleton Plus_op
        If {} -> S.singleton If_op
        Fold {} -> S.singleton Fold_op
        _ -> S.empty

restrictionSeries :: Monad m => Series m [OpName]
restrictionSeries =
  generate $ \_ -> filterM (const [False,True]) [Not_op, Shl1_op, Shr1_op, Shr4_op, Shr16_op, And_op, Or_op, Xor_op, Plus_op, If_op, Fold_op]

checkRestriction :: [OpName] -> ExpC -> Bool
checkRestriction strs e =
  expOps (expr e) `S.isSubsetOf` (S.fromList strs)
