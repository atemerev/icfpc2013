{-# LANGUAGE ImplicitParams #-}
module Gen (leftRightZerosUnop, leftRightZerosBinop, generateRestricted, generateRestrictedUpTo, serProg, serExpression', noRestriction, restrictionFromList, OpName(..)) where

import Types
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Bits
import ParSearch
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
-- Fold interpreted as unop over its body
leftRightZerosUnop lza rza Fold{} = (lza, rza)

leftRightZerosBinop :: Int -> Int -> Int -> Int -> Exp -> (Int, Int)
leftRightZerosBinop lza rza lzb rzb And{} = (max lza lzb, max rza rzb)
leftRightZerosBinop lza rza lzb rzb Or{} = (min lza lzb, min rza rzb)
leftRightZerosBinop lza rza lzb rzb Xor{} = (0, 0) -- no guarantees
leftRightZerosBinop lza rza lzb rzb Plus{} = (max (min lza lzb - 1) 0, min rza rzb)
-- If interpreted as binop over its two branches
leftRightZerosBinop lza rza lzb rzb If{} = (min lza lzb, min rza rzb)

-- Generators are restricted to allowed function set
data OpName = Not_op | Shl1_op | Shr1_op | Shr4_op
            | Shr16_op | And_op | Or_op | Xor_op
            | Plus_op | If_op | Fold_op
            deriving (Eq, Ord, Show)
data Restriction = Restriction { allowedOps :: !Int, nonzeroLeftBits :: !Int, nonzeroRightBits :: !Int } deriving (Eq, Show)
noRestriction = Restriction { allowedOps = 0xFFFF, nonzeroLeftBits = 0, nonzeroRightBits = 0 }

hasRestriction :: Restriction -> Restriction -> Bool
hasRestriction base elem = allowedOps base .&. allowedOps elem /= 0

removeOpRestriction :: Restriction -> OpName -> Restriction
removeOpRestriction base op = base { allowedOps = allowedOps base .&. (complement (restrictionMaskFromOp op)) }

allowed restriction op = hasRestriction restriction (restrictionFromOp op)

allow restriction opName f =
  if allowed restriction opName then Just f else Nothing

allowUnary restriction opName f =
  if allowedUnary restriction opName then Just f else Nothing

allowedUnary r@(Restriction _ lnz rnz) opName = allowed r opName && case opName of {
    Shl1_op -> rnz < 1
  ; Shr1_op -> lnz < 1
  ; Shr4_op -> lnz < 4
  ; Shr16_op -> lnz < 16
  ; _ -> True
  }

restrictionFromOp :: OpName -> Restriction
restrictionFromOp op = noRestriction { allowedOps = restrictionMaskFromOp op }

restrictionMaskFromOp :: OpName -> Int
restrictionMaskFromOp Not_op = 1 `shiftL` 0
restrictionMaskFromOp Shl1_op = 1 `shiftL` 1
restrictionMaskFromOp Shr1_op = 1 `shiftL` 2
restrictionMaskFromOp Shr4_op = 1 `shiftL` 3
restrictionMaskFromOp Shr16_op = 1 `shiftL` 4
restrictionMaskFromOp And_op = 1 `shiftL` 5
restrictionMaskFromOp Or_op = 1 `shiftL` 6
restrictionMaskFromOp Xor_op = 1 `shiftL` 7
restrictionMaskFromOp Plus_op = 1 `shiftL` 8
restrictionMaskFromOp If_op = 1 `shiftL` 9
restrictionMaskFromOp Fold_op = 1 `shiftL` 10

restrictionFromList rs = noRestriction { allowedOps = sum $ map restrictionMaskFromOp rs }

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

generateRestrictedUpTo :: MonadLevel m => Int -> [String] -> m ExpC -- allowed ops are passed as string list
generateRestrictedUpTo n rst = elements [1..n] >>= \i -> generateRestricted i rst

generateRestricted :: MonadLevel m => Int -> [String] -> m ExpC -- allowed ops are passed as string list
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

generateRestricted' :: MonadLevel m => Bool -> Int -> Restriction -> m ExpC
generateRestricted' tfold n restriction = 
  if tfold
  then
    let ?tfold = True
    in (\e -> fold_ mainArg zero e) `liftM` foldBodies (n-5) -- |fold x 0| is 2 + 1 + 1, hence n-4, and another -1 for top-level lambda
  else serProg n restriction
  where
    foldBodies :: (?tfold :: Bool, MonadLevel m) => Int -> m ExpC
    foldBodies n = do
      let filledCache = let ?cache = M.empty
                        in M.fromList [((i, InFoldBody), [p | p@(e,f,_,_) <- serExp' i restriction InFoldBody,
                                                                             isSimpleC e, usesFold2Arg e, usesFold1Arg e])
                                      | i <- [cacheMin .. min n cacheMax]
                                      ]
      let ?cache = filledCache
      (e, _, _, _) <- serExp' n (restriction `removeOpRestriction` Fold_op) InFoldBody -- Fold should not be there, but remove it just in case
      guard $ usesFold2Arg e && usesFold1Arg e -- since initial value for acc in tfold is known, bodies that use just acc are not interesting
      return e


allowedUnaryOps :: Restriction -> [ExpC -> ExpC]
allowedUnaryOps r = 
  catMaybes [ allowUnary r op f | (op,f) <- [(Not_op, not_), (Shl1_op, shl1), (Shr1_op, shr1), (Shr4_op, shr4), (Shr16_op, shr16)]]

allowedBinaryOps :: Restriction -> [ExpC -> ExpC -> ExpC]
allowedBinaryOps r = 
  catMaybes [ allow r op f | (op,f) <- [(And_op, and_), (Or_op, or_), (Xor_op, xor_), (Plus_op, plus)]]

allowedIf :: Restriction -> Bool
allowedIf r = allowed r If_op
  
allowedFold :: Restriction -> Bool
allowedFold r = allowed r Fold_op
      
-- 
serProg :: MonadLevel m => Int -> Restriction -> m ExpC
serProg n restriction = serExpression (n-1) restriction-- remove 1 level of depth for top-level lambda

-- a top-level expression, no tfold
serExpression :: (MonadLevel m) => Int -> Restriction -> m ExpC
serExpression n restriction = do
  (e,_,_,_) <- (serExpression' n restriction)
  return e

serExpression' :: (MonadLevel m) => Int -> Restriction -> m (ExpC, Bool, Int, Int)
serExpression' n restriction = do
  let ?tfold = False
  let filledCache = let ?cache = M.empty
                    in M.fromList [((i, fs), [p | p@(e,f,_,_) <- (serExp' i restriction fs), isSimpleC e, fs /= InFoldBody || usesFold2Arg e])
                                  | i <- [cacheMin .. min n cacheMax],
                                    fs <- [NoFold, ExternalFold, InFoldBody]
                                   ]
  let ?cache = filledCache
  serExp' n restriction NoFold

data FoldState = NoFold -- Allowed to generate unrestricted expression except references to fold args
               | ExternalFold -- Not allowed to generate folds or references to fold args
               | InFoldBody -- Allowed to generate references to fold args but not folds
               deriving (Eq, Show, Ord)

elements :: (MonadLevel m) => [a] -> m a
elements = msum . map return

type Cache = M.Map (Int, FoldState) [(ExpC, Bool, Int, Int)]
cacheMin = 3
cacheMax = 7

-- Generates (expression, does it contain a fold?, how many left bits are zero?, how many right bits are zero?)
serExp' :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> m (ExpC, Bool, Int, Int)
serExp' n _ _ | n < 1 = mzero
-- if tfold is set, the only occurrence of MainArg is at the toplevel
serExp' 1 _ InFoldBody = if ?tfold
                         then msum [return (zero, False, 64, 64), return (one, False, 63, 0),
                                    return (fold1Arg, False, 0, 0), return (fold2Arg, False, 0, 0)]
                         else msum [return (mainArg, False, 0, 0),
                                    return (zero, False, 64, 64), return (one, False, 63, 0),
                                    return (fold1Arg, False, 0, 0), return (fold2Arg, False, 0, 0)]
serExp' 1 _ _ = msum [return (zero, False, 64, 64), return (one, False, 63, 0), return (mainArg, False, 0, 0)]
serExp' 2 restriction fs = msum $ map (\op -> serUnop 2 restriction fs op) (allowedUnaryOps restriction)
serExp' n restriction fs
  | Just es <- M.lookup (n, fs) ?cache = elements es
serExp' 3 restriction fs = msum $ concat [
  map (\op -> serUnop 3 restriction fs op) (allowedUnaryOps restriction),
  map (\op -> serBinop 3 restriction fs op) (allowedBinaryOps restriction)]
serExp' n restriction fs = msum $ concat [
  if (n >= 4 && allowedIf restriction) then [serIf n restriction fs] else [],
  if (n >= 5 && fs == NoFold && allowedFold restriction) then [serFold n (restriction `removeOpRestriction` Fold_op)] else [],
  map (\op -> serUnop n restriction fs op) (allowedUnaryOps restriction),
  map (\op -> serBinop n restriction fs op) (allowedBinaryOps restriction)]

serIf :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> m (ExpC, Bool, Int, Int)
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
      let e = if0 a b c
      if isSimpleHead (expr e)
        then let (lze, rze) = leftRightZerosBinop lzb rzb lzc rzc (expr e)
             in return (e, foldA || foldB || foldC, lze, rze)
        else mzero

serFold :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> m (ExpC, Bool, Int, Int)
serFold n restriction = level $ do
  sizeArg <- elements [1..n - 4]
  sizeSeed <- elements [1..n - 3 - sizeArg]
  let sizeBody = n - 2 - sizeArg - sizeSeed
  (c, foldC, lzc, rzc) <- serExp' sizeBody restriction InFoldBody
  guard (usesFold2Arg c)
  -- TODO: is this really true that for tfold we could have ridiculous bodies that do not use foldAcc?
  (a, foldA, _, _) <- serExp' sizeArg restriction ExternalFold
  (b, foldB, _, _) <- serExp' sizeSeed restriction ExternalFold
  let e = fold_ a b c
  if isSimpleHead (expr e)
    then let (lze, rze) = leftRightZerosUnop lzc rzc (expr e)
         in return (e, True, lze, rze)
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

serUnop :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> (ExpC -> ExpC) -> m (ExpC, Bool, Int, Int)
serUnop n restriction fs op = level $ do
  (a, foldA, lza, rza) <- serExp' (n-1) restriction fs
  let e = op a
  if isSimpleHead (expr e)
    then let (lzu, rzu) = leftRightZerosUnop lza rza (expr e) in return (e, foldA, lzu, rzu)
    else mzero

serBinop :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> (ExpC -> ExpC -> ExpC) -> m (ExpC, Bool, Int, Int)
serBinop n restriction fs op = level $ do
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
        then let (lze, rze) = leftRightZerosBinop lza rza lzb rzb (expr e) in return (op a b, foldA || foldB, lze, rze)
        else mzero
