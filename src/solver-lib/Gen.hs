{-# LANGUAGE ImplicitParams #-}
module Gen (generateRestricted, generateRestrictedUpTo, serProg, noRestriction, restrictionFromList, OpName(..)) where

import Types
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Bits
import qualified Data.Map as M

import Debug.Trace

-- Traits of function:
-- * If at least one out has bit 0 = 1, cannot generate top-level shl or fold/shl or and (shl, _)
-- * If at least one out has set bits 63..[63, 60, 48], cannot generate same with shr
--   * (propagate mustVaryXXXBits through Fold, And)
-- "Must vary left X bits and right Y bits":
--   Zero:     reject if l > 0 or r > 0
--   One:      reject if l < 64
--   Not  (a): no constraint
--   Shl1 (a): l, r -> l-1, r+1 where r > 1
--   Shr1 (a): l, r -> l+1, r-1 where l > 1
--   Shr4 (a): l, r -> l+4, r-4 where l > 4
--   Shr16(a): l, r -> l+16,r-16 where l > 16
--      to have non-zero left l bits in Shr16(a), must have non-zero left l+16 bits in a; where l > 16
--   And(a,b): l, r -> l, r
--   Everything else: skip.

leftRightZerosUnop :: Int -> Int -> Exp -> (Int, Int)
leftRightZerosUnop lza rza Not{} = (0, 0)
leftRightZerosUnop lza rza Shl1{} = (max (lza-1) 0, min (rza+1) 64)
leftRightZerosUnop lza rza Shr1{} = (min (lza+1) 64, max (rza-1) 0)
leftRightZerosUnop lza rza Shr4{} = (min (lza+4) 64, max (rza-4) 0)
leftRightZerosUnop lza rza Shr16{} = (min (lza+16) 64, max (rza-16) 0)

leftRightZerosBinop :: Int -> Int -> Int -> Int -> Exp -> (Int, Int)
leftRightZerosBinop lza rza lzb rzb And{} = (max lza lzb, max rza rzb)
leftRightZerosBinop lza rza lzb rzb Or{} = (min lza lzb, min rza rzb)
leftRightZerosBinop lza rza lzb rzb Xor{} = (0, 0) -- no guarantees
leftRightZerosBinop lza rza lzb rzb Plus{} = (max (min lza lzb - 1) 0, min rza rzb)
leftRightZerosBinop lza rza lzb rzb If{} = (min lza lzb, min rza rzb)

-- Generators are restricted to allowed function set
data OpName = Not_op | Shl1_op | Shr1_op | Shr4_op
            | Shr16_op | And_op | Or_op | Xor_op
            | Plus_op | If_op | Fold_op
            deriving (Eq, Ord, Show)
type Restriction = Int
noRestriction = 0xFFFF :: Int

allowed restriction op = hasRestriction restriction (restrictionFromOp op)

allow restriction opName f =
  if allowed restriction opName then Just f else Nothing

restrictionFromOp :: OpName -> Restriction
restrictionFromOp Not_op = 1 `shiftL` 0
restrictionFromOp Shl1_op = 1 `shiftL` 1
restrictionFromOp Shr1_op = 1 `shiftL` 2
restrictionFromOp Shr4_op = 1 `shiftL` 3
restrictionFromOp Shr16_op = 1 `shiftL` 4
restrictionFromOp And_op = 1 `shiftL` 5
restrictionFromOp Or_op = 1 `shiftL` 6
restrictionFromOp Xor_op = 1 `shiftL` 7
restrictionFromOp Plus_op = 1 `shiftL` 8
restrictionFromOp If_op = 1 `shiftL` 9
restrictionFromOp Fold_op = 1 `shiftL` 10

mustVaryRightBit :: Restriction
mustVaryRightBit = 1 `shiftL` 11

mustVaryLeftBit :: Restriction
mustVaryLeftBit = 1 `shiftL` 12

mustVaryLeft4Bits :: Restriction
mustVaryLeft4Bits = 1 `shiftL` 13

mustVaryLeft16Bits :: Restriction
mustVaryLeft16Bits = 1 `shiftL` 14

restrictionFromList rs = sum $ map restrictionFromOp rs

addRestriction :: Restriction -> Restriction -> Restriction
addRestriction = (.|.)

hasRestriction :: Restriction -> Restriction -> Bool
hasRestriction base elem = base .&. elem /= 0

removeRestriction :: Restriction -> Restriction -> Restriction
removeRestriction base elem = base .&. (complement elem)

isSimpleC :: ExpC -> Bool
isSimpleC ec = isSimple (expr ec)

isSimple :: Exp -> Bool

isSimple Zero = True
isSimple One = True
isSimple e | isConstExpr e && (\v -> v == 0 || v == 1) (eval 0 0 0 (ExpC nothing64 e)) = False
isSimple e = isSimpleHead e && isSimpleParts e

isSimpleHead Zero = True
isSimpleHead One = True
isSimpleHead MainArg = True
isSimpleHead Fold1Arg = True
isSimpleHead Fold2Arg = True

isSimpleHead (If (ExpC _ MainArg) (ExpC _ MainArg) c) = False -- equal to (if0 MainArg 0 c)
isSimpleHead (If (ExpC _ Fold1Arg) (ExpC _ Fold1Arg) c) = False -- equal to (if0 Fold1Arg 0 c)
isSimpleHead (If (ExpC _ Fold2Arg) (ExpC _ Fold2Arg) c) = False -- equal to (if0 Fold2Arg 0 c)
isSimpleHead (If (ExpC _ (Not a)) b c) = False
isSimpleHead (If a b c) | b == c = False -- equal to b
isSimpleHead (If a b c) = True

isSimpleHead (Fold a b c) = True

isSimpleHead (Not (ExpC _ (Not a))) = False
isSimpleHead (Not a) = True

isSimpleHead (Shl1 (ExpC _ Zero)) = False
isSimpleHead (Shl1 (ExpC _ (Shr4 a))) = False -- symmetric to shr4 (shl1 ..)
isSimpleHead (Shl1 (ExpC _ (Shr16 a))) = False -- symmetric to shr16 (shl1 ..)
isSimpleHead (Shl1 a) = True

isSimpleHead (Shr1 (ExpC _ Zero)) = False
isSimpleHead (Shr1 (ExpC _ One))  = False
isSimpleHead (Shr1 a) = True

isSimpleHead (Shr4 (ExpC _ Zero)) = False
isSimpleHead (Shr4 (ExpC _ One))  = False
isSimpleHead (Shr4 (ExpC _ (Shr1 a))) = False
isSimpleHead (Shr4 a) = True

isSimpleHead (Shr16 (ExpC _ Zero)) = False
isSimpleHead (Shr16 (ExpC _ One))  = False
isSimpleHead (Shr16 (ExpC _ (Shr1 a))) = False
isSimpleHead (Shr16 (ExpC _ (Shr4 a))) = False
isSimpleHead (Shr16 a) = True

isSimpleHead (And (ExpC _ Zero) b) = False
isSimpleHead (And a (ExpC _ Zero)) = False
-- Normal form: first operand must be smaller in size
isSimpleHead (And a b) | expCSize a > expCSize b || a >= b = False
isSimpleHead (And a b) = True

isSimpleHead (Or (ExpC _ Zero) b) = False
isSimpleHead (Or a (ExpC _ Zero)) = False
isSimpleHead (Or a b) | expCSize a > expCSize b || a >= b = False
isSimpleHead (Or a b) = True

isSimpleHead (Xor (ExpC _ Zero) b) = False
isSimpleHead (Xor a (ExpC _ Zero)) = False
isSimpleHead (Xor a b) | expCSize a > expCSize b || a >= b = False
isSimpleHead (Xor a b) = True

isSimpleHead (Plus (ExpC _ Zero) b) = False
isSimpleHead (Plus a (ExpC _ Zero)) = False
isSimpleHead (Plus a b) | expCSize a > expCSize b || a >= b = False
isSimpleHead (Plus a b) = True

isSimpleParts Zero = True
isSimpleParts One = True
isSimpleParts MainArg = True
isSimpleParts Fold1Arg = True
isSimpleParts Fold2Arg = True
isSimpleParts (If a b c) = isSimpleC a && isSimpleC b && isSimpleC c
isSimpleParts (Fold a b c) = isSimpleC a && isSimpleC b && isSimpleC c
isSimpleParts (Not a) = isSimpleC a
isSimpleParts (Shl1 a) = isSimpleC a
isSimpleParts (Shr1 a) = isSimpleC a
isSimpleParts (Shr4 a) = isSimpleC a
isSimpleParts (Shr16 a) = isSimpleC a
isSimpleParts (And a b) = isSimpleC a && isSimpleC b
isSimpleParts (Or a b) = isSimpleC a && isSimpleC b
isSimpleParts (Xor a b) = isSimpleC a && isSimpleC b
isSimpleParts (Plus a b) = isSimpleC a && isSimpleC b

generateRestrictedUpTo :: Int -> [String] -> [ExpC] -- allowed ops are passed as string list
generateRestrictedUpTo n rst = concat [generateRestricted i rst | i <- [1..n]]

generateRestricted :: Int -> [String] -> [ExpC] -- allowed ops are passed as string list
generateRestricted n rst = 
  generateRestricted' tfold n restriction
  where
    tfold = "tfold" `elem` rst
    restriction = restrictionFromList $ map parse $ filter (/="bonus") $ filter (/="tfold") rst
    parse "not" = Not_op
    parse "shl1" = Shl1_op
    parse "shr1" = Shr1_op
    parse "shr4" = Shr4_op
    parse "shr16" = Shr16_op
    parse "and" = And_op
    parse "or" = Or_op
    parse "xor" = Xor_op
    parse "plus" = Plus_op
    parse "if0" = If_op
    parse "fold" = Fold_op
    parse other = error $ "failed to parse operation " ++ other

generateRestricted' :: Bool -> Int -> Restriction -> [ExpC]
generateRestricted' tfold n restriction = 
  if tfold
  then
    let ?tfold = True
    in map (\e -> fold_ mainArg zero e) $ list (n-5) foldBodies -- |fold x 0| is 2 + 1 + 1, hence n-4, and another -1 for top-level lambda
  else list n (serProg restriction)
  where
    foldBodies :: (?tfold :: Bool, Monad m) => Series m ExpC
    foldBodies = do
      n <- getDepth
      let filledCache = let ?cache = M.empty
                        in M.fromList [((i, InFoldBody), [p | p@(e,f,_,_) <- list i (serExp' i restriction InFoldBody),
                                                                             isSimpleC e, usesFold2Arg e, usesFold1Arg e])
                                      | i <- [cacheMin .. min n cacheMax]
                                      ]
      let ?cache = filledCache
      (e, _, _, _) <- serExp' n (restriction `removeRestriction` (restrictionFromOp Fold_op)) InFoldBody -- Fold should not be there, but remove it just in case
      guard $ usesFold2Arg e && usesFold1Arg e -- since initial value for acc in tfold is known, bodies that use just acc are not interesting
      return e


allowedUnaryOps :: Restriction -> [ExpC -> ExpC]
allowedUnaryOps r = 
  catMaybes [ allow r op f | (op,f) <- [(Not_op, not_), (Shl1_op, shl1), (Shr1_op, shr1), (Shr4_op, shr4), (Shr16_op, shr16)]]

allowedBinaryOps :: Restriction -> [ExpC -> ExpC -> ExpC]
allowedBinaryOps r = 
  catMaybes [ allow r op f | (op,f) <- [(And_op, and_), (Or_op, or_), (Xor_op, xor_), (Plus_op, plus)]]

allowedIf :: Restriction -> Bool
allowedIf r = allowed r If_op
  
allowedFold :: Restriction -> Bool
allowedFold r = allowed r Fold_op
      
-- 
serProg :: Monad m => Restriction -> Series m ExpC
serProg restriction = decDepth (serExpression restriction)-- remove 1 level of depth for top-level lambda

-- a top-level expression, no tfold
serExpression :: (Monad m) => Restriction -> Series m ExpC
serExpression restriction = do
  let ?tfold = False
  n <- getDepth
  let filledCache = let ?cache = M.empty
                    in M.fromList [((i, fs), [p | p@(e,f,_,_) <- (list i (serExp' i restriction fs)), isSimpleC e, fs /= InFoldBody || usesFold2Arg e])
                                  | i <- [cacheMin .. min n cacheMax],
                                    fs <- [NoFold, ExternalFold, InFoldBody]
                                   ]
  let ?cache = filledCache
  (e, _, _, _) <- serExp' n restriction NoFold
  return e

data FoldState = NoFold -- Allowed to generate unrestricted expression except references to fold args
               | ExternalFold -- Not allowed to generate folds or references to fold args
               | InFoldBody -- Allowed to generate references to fold args but not folds
               deriving (Eq, Show, Ord)

elements :: (MonadPlus m) => [a] -> m a
elements = msum . map return

type Cache = M.Map (Int, FoldState) [(ExpC, Bool, Int, Int)]
cacheMin = 3
cacheMax = 7

-- Generates (expression, does it contain a fold?, how many left bits are zero?, how many right bits are zero?)
serExp' :: (MonadPlus m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> m (ExpC, Bool, Int, Int)
serExp' n _ _ | n < 1 = mzero
-- if tfold is set, the only occurrence of MainArg is at the toplevel
serExp' 1 _ InFoldBody = if ?tfold
                         then msum [return (zero, False, 64, 64), return (one, False, 63, 1),
                                    return (fold1Arg, False, 0, 0), return (fold2Arg, False, 0, 0)]
                         else msum [return (mainArg, False, 0, 0),
                                    return (zero, False, 64, 64), return (one, False, 63, 1),
                                    return (fold1Arg, False, 0, 0), return (fold2Arg, False, 0, 0)]
serExp' 1 _ _ = msum [return (zero, False, 64, 64), return (one, False, 63, 61), return (mainArg, False, 0, 0)]
serExp' 2 restriction fs = msum $ map (\op -> serUnop 2 restriction fs op) (allowedUnaryOps restriction)
serExp' n restriction fs
  | Just es <- M.lookup (n, fs) ?cache = elements es
serExp' 3 restriction fs = msum $ concat [
  map (\op -> serUnop 3 restriction fs op) (allowedUnaryOps restriction),
  map (\op -> serBinop 3 restriction fs op) (allowedBinaryOps restriction)]
serExp' n restriction fs = msum $ concat [
  if (n >= 4 && allowedIf restriction) then [serIf n restriction fs] else [],
  if (n >= 5 && fs == NoFold && allowedFold restriction) then [serFold n (restriction `removeRestriction` (restrictionFromOp Fold_op))] else [],
  map (\op -> serUnop n restriction fs op) (allowedUnaryOps restriction),
  map (\op -> serBinop n restriction fs op) (allowedBinaryOps restriction)]

serIf :: (MonadPlus m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> m (ExpC, Bool, Int, Int)
serIf n restriction fs = do
  sizeA <- elements [1..n - 3]
  (a, foldA, _, _) <- serExp' sizeA restriction fs
  if isConstExprC a
    then mzero
    else do
      sizeB <- elements [1..n - 2 - sizeA]
      let sizeC = n - 1 - sizeA - sizeB
      (b, foldB, lzb, rzb) <- serExp' sizeB restriction (if foldA then ExternalFold else fs)
      (c, foldC, lzc, rzc) <- serExp' sizeC restriction (if (foldA || foldB) then ExternalFold else fs)
      if isSimpleHead (If a b c)
        then return (if0 a b c, foldA || foldB || foldC, min lzb lzc, min rzb rzc)
        else mzero

serFold :: (MonadPlus m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> m (ExpC, Bool, Int, Int)
serFold n restriction = do
  sizeArg <- elements [1..n - 4]
  sizeSeed <- elements [1..n - 3 - sizeArg]
  let sizeBody = n - 2 - sizeArg - sizeSeed
  (c, foldC, lzc, rzc) <- serExp' sizeBody restriction InFoldBody
  guard (usesFold2Arg c)
  -- TODO: is this really true that for tfold we could have ridiculous bodies that do not use foldAcc?
  (a, foldA, _, _) <- serExp' sizeArg restriction ExternalFold
  (b, foldB, _, _) <- serExp' sizeSeed restriction ExternalFold
  if isSimpleHead (Fold a b c)
    then return (fold_ a b c, True, lzc, rzc)
    else mzero
    
usesFold2Arg :: ExpC -> Bool
usesFold2Arg (ExpC _ e) = u e
  where
    u Zero = False
    u One = False
    u MainArg = False
    u Fold1Arg = False
    u Fold2Arg = True
    u (If a b c) = {- usesFold2Arg a || -} usesFold2Arg b || usesFold2Arg c -- if0 branches should refer to fold2Arg, otherwise they are as good as constant
    u (Fold a b c) = usesFold2Arg a || usesFold2Arg b || usesFold2Arg c -- should not happen, but still
    u (Not a) = usesFold2Arg a
    u (Shl1 a) = usesFold2Arg a
    u (Shr1 a) = usesFold2Arg a
    u (Shr4 a) = usesFold2Arg a
    u (Shr16 a) = usesFold2Arg a
    u (And a b) = usesFold2Arg a || usesFold2Arg b
    u (Or a b) = usesFold2Arg a || usesFold2Arg b
    u (Xor a b) = usesFold2Arg a || usesFold2Arg b
    u (Plus a b) = usesFold2Arg a || usesFold2Arg b

usesFold1Arg :: ExpC -> Bool
usesFold1Arg (ExpC _ e) = u e
  where
    u Zero = False
    u One = False
    u MainArg = False
    u Fold1Arg = True
    u Fold2Arg = False
    u (If a b c) = {- usesFold1Arg a || -} usesFold1Arg b || usesFold1Arg c -- if0 branches should refer to fold2Arg, otherwise they are as good as constant
    u (Fold a b c) = usesFold1Arg a || usesFold1Arg b || usesFold1Arg c -- should not happen, but still
    u (Not a) = usesFold1Arg a
    u (Shl1 a) = usesFold1Arg a
    u (Shr1 a) = usesFold1Arg a
    u (Shr4 a) = usesFold1Arg a
    u (Shr16 a) = usesFold1Arg a
    u (And a b) = usesFold1Arg a || usesFold1Arg b
    u (Or a b) = usesFold1Arg a || usesFold1Arg b
    u (Xor a b) = usesFold1Arg a || usesFold1Arg b
    u (Plus a b) = usesFold1Arg a || usesFold1Arg b

serUnop :: (MonadPlus m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> (ExpC -> ExpC) -> m (ExpC, Bool, Int, Int)
serUnop n restriction fs op = do
  (a, foldA, lza, rza) <- serExp' (n-1) restriction fs
  let e = op a
  if isSimpleHead (expr e)
    then let (lzu, rzu) = leftRightZerosUnop lza rza (expr e) in return (e, foldA, lzu, rzu)
    else mzero

serBinop :: (MonadPlus m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> (ExpC -> ExpC -> ExpC) -> m (ExpC, Bool, Int, Int)
serBinop n restriction fs op = do
  sizeA <- elements [1..n - 2]
  let sizeB = n - 1 - sizeA
  -- Normal form: first operand must be smaller in size
  if sizeA > sizeB
    then mzero
    else do
      (a, foldA, lza, rza) <- serExp' sizeA restriction fs
      guard $ expr a /= Zero -- all binary ops (and, or, xor, plus) are stupid if first arg is zero
      (b, foldB, lzb, rzb) <- serExp' sizeB restriction (if foldA then ExternalFold else fs)
      let e = op a b
      if isSimpleHead (expr e)
        then let (lze, rze) = leftRightZerosBinop lza lzb rza rzb (expr e) in return (op a b, foldA || foldB, lze, rze)
        else mzero
