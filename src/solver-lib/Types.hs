{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Types
  ( Exp(..)
  , Word64
  , eval
  , serProg
  )
  where

import Data.Word
import Data.Bits
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad

data Exp where
  Zero :: Exp
  One :: Exp
  MainArg :: Exp
  Fold1Arg :: Exp
  Fold2Arg :: Exp
  If :: { ifCond :: Exp, ifTrue :: Exp, ifFalse :: Exp } -> Exp
  Fold :: { foldArg :: Exp, foldSeed :: Exp, foldBody :: Exp } -> Exp
  Not :: Exp -> Exp
  Shl1 :: Exp -> Exp
  Shr1 :: Exp -> Exp
  Shr4 :: Exp -> Exp
  Shr16 :: Exp -> Exp
  And :: Exp -> Exp -> Exp
  Or :: Exp -> Exp -> Exp
  Xor :: Exp -> Exp -> Exp
  Plus :: Exp -> Exp -> Exp
  deriving (Eq, Show)

eval :: Word64 -> Word64 -> Word64 -> Exp -> Word64
eval main fold1 fold2 e = eval' e
  where eval' e = case e of {
          Zero -> 0
        ; One -> 1
        ; MainArg -> main
        ; Fold1Arg -> fold1
        ; Fold2Arg -> fold2
        ; If c t f -> if eval' c == 0  -- TODO: Maybe /= 0, need to check.
                      then eval' t
                      else eval' f
        ; Not e -> complement (eval' e)
        ; Shl1 e -> shiftL (eval' e) 1
        ; Shr1 e -> shiftR (eval' e) 1
        ; Shr4 e -> shiftR (eval' e) 4
        ; Shr16 e -> shiftR (eval' e) 16
        ; And e1 e2 -> eval' e1 .&. eval' e2
        ; Or e1 e2 -> eval' e1 .|. eval' e2
        ; Xor e1 e2 -> xor (eval' e1) (eval' e2)
        ; Plus e1 e2 -> eval' e1 + eval' e2
        ; Fold arg seed body -> foldImpl (eval' arg) (eval' seed) body
        }
        foldImpl x seed body = op x0 (op x1 (op x2 (op x3 (op x4 (op x5 (op x6 (op x7 seed)))))))
          where
            op a b = eval main a b body
            -- x7 is least significant, x0 most significant
            (x7', x7) = x `divMod` 256
            (x6', x6) = x7' `divMod` 256
            (x5', x5) = x6' `divMod` 256
            (x4', x4) = x5' `divMod` 256
            (x3', x3) = x4' `divMod` 256
            (x2', x2) = x3' `divMod` 256
            (x1', x1) = x2' `divMod` 256
            (x0', x0) = x1' `divMod` 256

serProg :: Monad m => Series m Exp
serProg = decDepth series

instance (Monad m) => Serial m Exp where
  series = serExp

serExp :: (Monad m) => Series m Exp
serExp = do
  n <- getDepth
  (e, hasFold) <- serExp' n NoFold
  return e

data FoldState = NoFold -- Allowed to generate unrestricted expression except references to fold args
               | ExternalFold -- Not allowed to generate folds or references to fold args
               | InFoldBody -- Allowed to generate references to fold args but not folds
               deriving (Eq, Show)

oneof :: (Monad m) => [Series m a] -> Series m a
oneof xs = foldr mplus mzero xs

elements :: (Monad m) => [a] -> Series m a
elements = oneof . map return

-- Generates (expression, does it contain a fold?)
serExp' :: (Monad m) => Int -> FoldState -> Series m (Exp, Bool)
serExp' 1 InFoldBody = oneof $ map (\x -> return (x, False)) [Zero, One, MainArg, Fold1Arg, Fold2Arg]
serExp' 1 _ = oneof $ map (\x -> return (x, False)) [Zero, One, MainArg]
serExp' n fs = oneof $ concat [
  if n >= 4 then [serIf n fs] else [],
  if (n >= 5 && fs == NoFold) then [serFold n] else [],
  if n >= 2 then map (\op -> serUnop n fs op) [Not, Shl1, Shr1, Shr4, Shr16] else [], 
  if n >= 3 then map (\op -> serBinop n fs op) [And, Or, Xor, Plus] else []]

serIf :: (Monad m) => Int -> FoldState -> Series m (Exp, Bool)
serIf n fs = do
  sizeA <- elements [1..n - 3]
  sizeB <- elements [1..n - 2 - sizeA]
  let sizeC = n - 1 - sizeA - sizeB
  (a, foldA) <- serExp' sizeA fs
  (b, foldB) <- serExp' sizeB (if foldA then ExternalFold else fs)
  (c, foldC) <- serExp' sizeC (if (foldA || foldB) then ExternalFold else fs)
  return (If a b c, foldA || foldB || foldC)

serFold :: (Monad m) => Int -> Series m (Exp, Bool)
serFold n = do
  sizeArg <- elements [1..n - 4]
  sizeSeed <- elements [1..n - 3 - sizeArg]
  let sizeBody = n - 2 - sizeArg - sizeSeed
  (a, foldA) <- serExp' sizeArg NoFold
  (b, foldB) <- serExp' sizeSeed (if foldA then ExternalFold else NoFold)
  (c, foldC) <- serExp' sizeBody (if (foldA || foldB) then ExternalFold else NoFold)
  return (Fold a b c, True)

serUnop :: (Monad m) => Int -> FoldState -> (Exp -> Exp) -> Series m (Exp, Bool)
serUnop n fs op = do
  (a, foldA) <- serExp' (n-1) fs
  return (op a, foldA)

serBinop :: (Monad m) => Int -> FoldState -> (Exp -> Exp -> Exp) -> Series m (Exp, Bool)
serBinop n fs op = do
  sizeA <- elements [1..n - 2]
  let sizeB = n - 1 - sizeA
  (a, foldA) <- serExp' sizeA fs
  (b, foldB) <- serExp' sizeB (if foldA then ExternalFold else fs)
  return (op a b, foldA || foldB)

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

allExpsAreValid = smallCheck 6 isValid
allExpsAreSpecifiedSize =
  smallCheck 6 $ \n ->
  over (generate $ \_ -> list n serProg) $ \prog ->
    progSize prog == n
