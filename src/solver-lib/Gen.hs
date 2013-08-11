{-# LANGUAGE ImplicitParams #-}
module Gen (parityUnop, parityBinop, parity, leftRightZerosUnop, leftRightZerosBinop, leftRightZeros, Restriction(..), generateRestricted, generateRestrictedUpTo, serProg, serExpression', noRestriction, restrictionFromList, OpName(..)) where

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

data Parity = PMain | PNegMain | PZero | POne | PUnknown deriving (Eq, Show, Ord)

pNegate :: Parity -> Parity
pNegate PMain    = PNegMain
pNegate PNegMain = PMain
pNegate PZero    = POne
pNegate POne     = PZero
pNegate PUnknown = PUnknown

parityUnop :: Parity -> Exp -> Parity
parityUnop p Not{} = pNegate p
parityUnop p Fold{} = p
parityUnop p Shl1{} = PZero
parityUnop p _ = PUnknown

parityBinop :: Parity -> Parity -> Exp -> Parity
parityBinop PMain    PMain    And{} = PMain
parityBinop PMain    PNegMain And{} = PZero
parityBinop PNegMain PMain    And{} = PZero
parityBinop PNegMain PNegMain And{} = PNegMain
parityBinop PZero    _        And{} = PZero
parityBinop POne     pb       And{} = pb
parityBinop _        PZero    And{} = PZero
parityBinop pb       POne     And{} = pb

parityBinop PMain    PMain    Or{} = PMain
parityBinop PMain    PNegMain Or{} = POne
parityBinop PNegMain PMain    Or{} = POne
parityBinop PNegMain PNegMain Or{} = PNegMain
parityBinop PZero    pb       Or{} = pb
parityBinop POne     _        Or{} = POne
parityBinop pb       PZero    Or{} = pb
parityBinop _        POne     Or{} = POne

parityBinop pa pb Xor{} = parityBinop pa pb (Plus undefined undefined)

parityBinop POne     pa       Plus{} = pNegate pa
parityBinop pa       POne     Plus{} = pNegate pa
parityBinop PZero    pa       Plus{} = pa
parityBinop pa       PZero    Plus{} = pa
parityBinop PMain    PMain    Plus{} = PZero
parityBinop PMain    PNegMain Plus{} = POne
parityBinop PNegMain PMain    Plus{} = POne
parityBinop PNegMain PNegMain Plus{} = PZero

parityBinop pa pb If{} = if pa == pb then pa else PUnknown
parityBinop _ _ _ = PUnknown

parity :: Exp -> Parity
parity Zero = PZero
parity One = POne
parity MainArg = PMain
parity Fold1Arg = PUnknown
parity Fold2Arg = PUnknown
parity e@(Not (ExpC _ a)) = parityUnop (parity a) e
parity e@(Shl1 (ExpC _ a)) = parityUnop (parity a) e
parity e@(Shr1 (ExpC _ a)) = parityUnop (parity a) e
parity e@(Shr4 (ExpC _ a)) = parityUnop (parity a) e
parity e@(Shr16 (ExpC _ a)) = parityUnop (parity a) e
parity e@(Fold _ _ (ExpC _ a)) = parityUnop (parity a) e
parity e@(And (ExpC _ a) (ExpC _ b)) = parityBinop (parity a) (parity b) e
parity e@(Or (ExpC _ a) (ExpC _ b)) = parityBinop (parity a) (parity b) e
parity e@(Xor (ExpC _ a) (ExpC _ b)) = parityBinop (parity a) (parity b) e
parity e@(Plus (ExpC _ a) (ExpC _ b)) = parityBinop (parity a) (parity b) e
parity e@(If _ (ExpC _ a) (ExpC _ b)) = parityBinop (parity a) (parity b) e

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
leftRightZerosBinop lza rza lzb rzb Xor{} = (min lza lzb, min rza rzb)
leftRightZerosBinop lza rza lzb rzb Plus{} = (max (min lza lzb - 1) 0, min rza rzb)
-- If interpreted as binop over its two branches
leftRightZerosBinop lza rza lzb rzb If{} = (min lza lzb, min rza rzb)

leftRightZeros :: Exp -> (Int, Int)
leftRightZeros Zero = (64, 64)
leftRightZeros One = (63, 0)
leftRightZeros MainArg = (0, 0)
leftRightZeros Fold1Arg = (0, 0)
leftRightZeros Fold2Arg = (0, 0)
leftRightZeros e@(Not (ExpC _ a)) = let (lza, rza) = leftRightZeros a in leftRightZerosUnop lza rza e
leftRightZeros e@(Shl1 (ExpC _ a)) = let (lza, rza) = leftRightZeros a in leftRightZerosUnop lza rza e
leftRightZeros e@(Shr1 (ExpC _ a)) = let (lza, rza) = leftRightZeros a in leftRightZerosUnop lza rza e
leftRightZeros e@(Shr4 (ExpC _ a)) = let (lza, rza) = leftRightZeros a in leftRightZerosUnop lza rza e
leftRightZeros e@(Shr16 (ExpC _ a)) = let (lza, rza) = leftRightZeros a in leftRightZerosUnop lza rza e
leftRightZeros e@(Fold _ _ (ExpC _ a)) = let (lza, rza) = leftRightZeros a in leftRightZerosUnop lza rza e
leftRightZeros e@(And (ExpC _ a) (ExpC _ b)) = let (lza, rza) = leftRightZeros a
                                                   (lzb, rzb) = leftRightZeros b
                                               in leftRightZerosBinop lza rza lzb rzb e
leftRightZeros e@(Or (ExpC _ a) (ExpC _ b)) = let (lza, rza) = leftRightZeros a
                                                  (lzb, rzb) = leftRightZeros b
                                              in leftRightZerosBinop lza rza lzb rzb e
leftRightZeros e@(Xor (ExpC _ a) (ExpC _ b)) = let (lza, rza) = leftRightZeros a
                                                   (lzb, rzb) = leftRightZeros b
                                               in leftRightZerosBinop lza rza lzb rzb e
leftRightZeros e@(Plus (ExpC _ a) (ExpC _ b)) = let (lza, rza) = leftRightZeros a
                                                    (lzb, rzb) = leftRightZeros b
                                                in leftRightZerosBinop lza rza lzb rzb e
leftRightZeros e@(If _ (ExpC _ a) (ExpC _ b)) = let (lza, rza) = leftRightZeros a
                                                    (lzb, rzb) = leftRightZeros b
                                                in leftRightZerosBinop lza rza lzb rzb e

-- Generators are restricted to allowed function set
data OpName = Not_op | Shl1_op | Shr1_op | Shr4_op
            | Shr16_op | And_op | Or_op | Xor_op
            | Plus_op | If_op | Fold_op
            deriving (Eq, Ord, Show)

applyUnop Not_op x = not_ x
applyUnop Shl1_op x = shl1 x
applyUnop Shr1_op x = shr1 x
applyUnop Shr4_op x = shr4 x
applyUnop Shr16_op x = shr16 x
applyBinop And_op a b = and_ a b
applyBinop Or_op a b = or_ a b
applyBinop Xor_op a b = xor_ a b
applyBinop Plus_op a b = plus a b


-- allowedZeroLeftBits: "don't generate functions which are guaranteed
-- to only return values with > allowedZeroLeftBits zero bits on the left"
data Restriction = Restriction { allowedOps :: !Int, allowedZeroLeftBits :: !Int, allowedZeroRightBits :: !Int } deriving (Eq, Show)
noRestriction = Restriction { allowedOps = 0xFFFF, allowedZeroLeftBits = 64, allowedZeroRightBits = 64 }

hasRestriction :: Restriction -> Restriction -> Bool
hasRestriction base elem = allowedOps base .&. allowedOps elem /= 0

removeOpRestriction :: Restriction -> OpName -> Restriction
removeOpRestriction base op = base { allowedOps = allowedOps base .&. (complement (restrictionMaskFromOp op)) }

allowed restriction op = hasRestriction restriction (restrictionFromOp op)

allowedUnary r@(Restriction _ alz arz) opName = allowed r opName && case opName of {
    Shl1_op -> arz >= 1
  ; Shr1_op -> alz >= 1
  ; Shr4_op -> alz >= 4
  ; Shr16_op -> alz >= 16
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
isSimpleHead (If (ExpC _ MainArg) (ExpC _ Zero) (ExpC _ MainArg)) = False -- equal to MainArg
isSimpleHead (If (ExpC _ Fold1Arg) (ExpC _ Zero) (ExpC _ Fold1Arg)) = False -- equal to Fold1Arg
isSimpleHead (If (ExpC _ Fold2Arg) (ExpC _ Zero) (ExpC _ Fold2Arg)) = False -- equal to Fold2Arg
-- isSimpleHead (If (ExpC _ (Not a)) b c) = False -- meant to be equal to (if0 a c b), but it's not the case, since (Not) is not a boolean op
isSimpleHead (If a b c) | b == c = False -- equal to b
isSimpleHead (If a b c) = True

isSimpleHead (Fold a b c) = True

isSimpleHead (Not (ExpC _ (Not a))) = False
isSimpleHead (Not a) = True

isSimpleHead (Shl1 (ExpC _ Zero)) = False
isSimpleHead (Shl1 (ExpC _ (Shr1 (ExpC _ (Shl1 a))))) = False -- equal to shl1 a
isSimpleHead (Shl1 (ExpC _ (And (ExpC _ (Not (ExpC _ One))) a))) = False -- equal to shl1 a
isSimpleHead (Shl1 (ExpC _ (And a (ExpC _ (Not (ExpC _ One)))))) = False -- equal to shl1 a
isSimpleHead (Shl1 a) = True

isSimpleHead (Shr1 (ExpC _ Zero)) = False
isSimpleHead (Shr1 (ExpC _ One))  = False
isSimpleHead (Shr1 (ExpC _ (Shl1 (ExpC _ (Shr1 a))))) = False -- equal to shr1 a
isSimpleHead (Shr1 a) = True

isSimpleHead (Shr4 (ExpC _ Zero)) = False
isSimpleHead (Shr4 (ExpC _ One))  = False
isSimpleHead (Shr4 (ExpC _ (Shr1 a))) = False -- symmetric to shr1 (shr4 ..)
isSimpleHead (Shr4 a) = True

isSimpleHead (Shr16 (ExpC _ Zero)) = False
isSimpleHead (Shr16 (ExpC _ One))  = False
isSimpleHead (Shr16 (ExpC _ (Shr1 a))) = False
isSimpleHead (Shr16 (ExpC _ (Shr4 a))) = False
isSimpleHead (Shr16 (ExpC _ (Shr16 (ExpC _ (Shr16 (ExpC _ (Shr16 a))))))) = False -- equal to 0
isSimpleHead (Shr16 a) = True

isSimpleHead (And (ExpC _ One) (ExpC _ (Shl1 a))) = False -- equal to Zero
isSimpleHead (And (ExpC _ (Shl1 a)) (ExpC _ One)) = False -- equal to Zero
isSimpleHead (And (ExpC _ Zero) b) = False
isSimpleHead (And a (ExpC _ Zero)) = False
-- Normal form: first operand must be smaller in size 
isSimpleHead (And a b) | (expCSize a, a) > (expCSize b, b) || a == b = False
isSimpleHead (And a b) = True
-- TODO: And (Not 1) a, And a (Not 1) can be replaced by (shl1 (shr1 a)), but we don't know if shr1 and shl1 are available
-- TODO: And (Not a) (Not b) can be replaced by (Not (Or a b)), but we don't know if Or is available
-- TODO: similar for (not (and (not a) (not b))), (not (and (not a) b)), etc.

isSimpleHead (Or (ExpC _ Zero) b) = False
isSimpleHead (Or a (ExpC _ Zero)) = False
isSimpleHead (Or a b) | (expCSize a, a) > (expCSize b, b) || a == b = False
-- TODO: same considerations as for And
isSimpleHead (Or a b) = True

isSimpleHead (Xor (ExpC _ Zero) b) = False
isSimpleHead (Xor a (ExpC _ Zero)) = False
isSimpleHead (Xor a b) | (expCSize a, a) > (expCSize b, b) || a == b = False
isSimpleHead (Xor a b) = True

isSimpleHead (Plus (ExpC _ Zero) b) = False
isSimpleHead (Plus a (ExpC _ Zero)) = False
isSimpleHead (Plus a b) | (expCSize a, a) > (expCSize b, b) = False -- No a==b case to allow (plus x x), which might be needed if shl1 is not available
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

generateRestrictedUpTo :: MonadLevel m => Int -> [String] -> (Int, Int) -> Bool -> Maybe Word64 -> m ExpC -- allowed ops are passed as string list
generateRestrictedUpTo n rst (alz, arz) unknownParityOnly valueConstraint = elements [1..n] >>= \i -> generateRestricted i rst (alz, arz) unknownParityOnly valueConstraint

generateRestricted :: MonadLevel m => Int -> [String] -> (Int, Int) -> Bool -> Maybe Word64 -> m ExpC -- allowed ops are passed as string list
generateRestricted n rst (alz, arz) unknownParityOnly valueConstraint = generateRestricted' bonus tfold n restriction unknownParityOnly valueConstraint
  where
    tfold = "tfold" `elem` rst
    bonus = "bonus" `elem` rst
    restriction = restriction0 { allowedZeroLeftBits = alz, allowedZeroRightBits = arz }
    restriction0 = restrictionFromList $ map parse $ filter (/="bonus") $ filter (/="tfold") rst
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

generateRestricted' :: MonadLevel m => Bool -> Bool -> Int -> Restriction -> Bool -> Maybe Word64 -> m ExpC
generateRestricted' bonus tfold n restriction unknownParityOnly valueConstraint = 
  -- All bonus tasks have the same form:
  -- 1)top-level expression is (if (and 1 a) b c)
  -- 2)there are no nested ifs
  -- 3)sizes of a, b, c are roughly equal - 5-9 for tasks of size 20-25
  --                                        9-15 for tasks of size 35-43
  if bonus
  then do (e,_,_,_,_) <- do let filledCache = 
                                  let ?cache = M.empty
                                      ?tfold = False
                                  in M.fromList [((i, ExternalFold), [p | p@(e,f,_,_,_) <- (serExp' i (restriction `removeOpRestriction` If_op) ExternalFold False Nothing), isSimpleC e])
                                                | i <- [5 .. 12]
                                                ]
                            let ?cache = M.empty
                                ?tfold = False -- always
                              in serIf True n restriction ExternalFold False valueConstraint
          return e
  else    
    if tfold
    then
      let ?tfold = True
      in (\e -> fold_ mainArg zero e) `liftM` foldBodies (n-5) -- |fold x 0| is 2 + 1 + 1, hence n-4, and another -1 for top-level lambda
    else serProg n restriction unknownParityOnly valueConstraint
  where
    foldBodies :: (?tfold :: Bool, MonadLevel m) => Int -> m ExpC
    foldBodies n = do
      let opsOnlyRestriction = noRestriction {allowedOps = allowedOps restriction}
      let filledCache = let ?cache = M.empty
                        in M.fromList [((i, InFoldBody), [p | p@(e,f,_,_,_) <- serExp' i opsOnlyRestriction InFoldBody False Nothing,
                                                                               isSimpleC e {-, usesFold2Arg e, usesFold1Arg e-} ])
                                      | i <- [cacheMin .. min n cacheMax]
                                      ]
      let ?cache = filledCache
      (e, _, _, _, _) <- serExp' n (restriction `removeOpRestriction` Fold_op) InFoldBody False Nothing -- TODO: vconstraint -- Fold should not be there, but remove it just in case
      -- guard $ usesFold2Arg e && usesFold1Arg e -- since initial value for acc in tfold is known, bodies that use just acc are not interesting
      return e


allowedUnaryOps :: Restriction -> [OpName]
allowedUnaryOps r = [op | op <- [Not_op, Shl1_op, Shr1_op, Shr4_op, Shr16_op], allowedUnary r op]

allowedBinaryOps :: Restriction -> [OpName]
allowedBinaryOps r = [op | op <- [And_op, Or_op, Xor_op, Plus_op], allowed r op]

allowedIf :: Restriction -> Bool
allowedIf r = allowed r If_op
  
allowedFold :: Restriction -> Bool
allowedFold r = allowed r Fold_op

-- 
serProg :: MonadLevel m => Int -> Restriction -> Bool -> Maybe Word64 -> m ExpC
serProg n restriction unknownParityOnly valueConstraint = serExpression (n-1) restriction unknownParityOnly valueConstraint -- remove 1 level of depth for top-level lambda

-- a top-level expression, no tfold
serExpression :: (MonadLevel m) => Int -> Restriction -> Bool -> Maybe Word64 -> m ExpC
serExpression n restriction unknownParityOnly valueConstraint = do
  (e,_,_,_,_) <- (serExpression' n restriction unknownParityOnly valueConstraint)
  return e

serExpression' :: (MonadLevel m) => Int -> Restriction -> Bool -> Maybe Word64 -> m (ExpC, Bool, Int, Int, Parity)
serExpression' n restriction unknownParityOnly valueConstraint = do
  let opsOnlyRestriction = noRestriction {allowedOps = allowedOps restriction}
  let ?tfold = False
  let filledCache = let ?cache = M.empty
                    in M.fromList [((i, fs), [p | p@(e,f,_,_,_) <- (serExp' i opsOnlyRestriction fs False Nothing), isSimpleC e, fs /= InFoldBody || usesFold1Arg e || usesFold2Arg e])
                                  | i <- [cacheMin .. min n cacheMax],
                                    fs <- [NoFold, ExternalFold, InFoldBody]
                                   ]
  let ?cache = filledCache
  serExp' n restriction NoFold unknownParityOnly valueConstraint

data FoldState = NoFold -- Allowed to generate unrestricted expression except references to fold args
               | ExternalFold -- Not allowed to generate folds or references to fold args
               | InFoldBody -- Allowed to generate references to fold args but not folds
               deriving (Eq, Show, Ord)

elements :: (MonadLevel m) => [a] -> m a
elements = msum . map return

type Cache = M.Map (Int, FoldState) [(ExpC, Bool, Int, Int, Parity)]
cacheMin = 3
cacheMax = 7

-- Generates (expression, does it contain a fold?, how many left bits are zero?, how many right bits are zero?)
serExp' :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> Bool -> Maybe Word64 -> m (ExpC, Bool, Int, Int, Parity)
serExp' n _ _ _ _ | n < 1 = mzero
-- if tfold is set, the only occurrence of MainArg is at the toplevel
serExp' 1 (Restriction _ alz arz) InFoldBody unknownParityOnly valueConstraint = do
  let maybeZero = if (not unknownParityOnly) && alz == 64 && arz == 64 && maybe True (==0) valueConstraint
                  then return (zero, False, 64, 64, PZero) else mzero
  let maybeOne  = if (not unknownParityOnly) && alz >= 63 && maybe True (==1) valueConstraint
                  then return (one, False, 63, 0, POne) else mzero
  let maybeMainArg = if (not unknownParityOnly) && maybe True (==(fromMWord64 (error "boo") $ cached mainArg)) valueConstraint
                     then return (mainArg, False, 0, 0, PMain) else mzero
  if ?tfold
  -- zero is allowed if alz == 64 && arz == 64
  -- one is allowed if alz >= 63
  then msum [maybeZero, maybeOne, return (fold1Arg, False, 0, 0, PUnknown), return (fold2Arg, False, 0, 0, PUnknown)]
  else msum [maybeMainArg, maybeZero, maybeOne,
             return (fold1Arg, False, 0, 0, PUnknown), return (fold2Arg, False, 0, 0, PUnknown)]
serExp' 1 (Restriction _ alz arz) _ unknownParityOnly valueConstraint = do
  let maybeZero = if (not unknownParityOnly) && alz == 64 && arz == 64 && maybe True (==0) valueConstraint
                  then return (zero, False, 64, 64, PZero) else mzero
  let maybeOne  = if (not unknownParityOnly) && alz >= 63 && maybe True (==1) valueConstraint
                  then return (one, False, 63, 0, POne) else mzero
  let maybeMainArg = if (not unknownParityOnly) && maybe True (==(fromMWord64 (error "boo") $ cached mainArg)) valueConstraint
                     then return (mainArg, False, 0, 0, PMain) else mzero
  msum [maybeZero, maybeOne, maybeMainArg]
serExp' 2 restriction fs unknownParityOnly valueConstraint = msum $ map (\op -> serUnop 2 restriction fs op unknownParityOnly valueConstraint) (allowedUnaryOps restriction)
serExp' n restriction@(Restriction _ alz arz) fs unknownParityOnly valueConstraint
  -- When taking from cache, remember that the cache was unrestricted by left/right bit zeroing.
  -- If we need to get entries which are allowed to zero at most alz bits, then omit entries which all zero 
  | Just es <- M.lookup (n, fs) ?cache = 
    let fitsConstraint e = 
          let eVal = cached e
              in if isNothing64 eVal then True else maybe True (==(fromMWord64 (error "zoo") eVal)) valueConstraint
    in
     elements (filter (\(e, _, lz, rz, py) -> lz <= alz && rz <= arz && fitsConstraint e ) es)
serExp' 3 restriction fs unknownParityOnly valueConstraint = msum $ concat [
  map (\op -> serUnop 3 restriction fs op unknownParityOnly valueConstraint) (allowedUnaryOps restriction),
  map (\op -> serBinop 3 restriction fs op unknownParityOnly valueConstraint) (allowedBinaryOps restriction)]
serExp' n restriction fs unknownParityOnly valueConstraint = msum $ concat [
  if (n >= 4 && allowedIf restriction) then [serIf False n restriction fs unknownParityOnly valueConstraint] else [],
  if (n >= 5 && fs == NoFold && allowedFold restriction) then [serFold n (restriction `removeOpRestriction` Fold_op) unknownParityOnly] else [],
  map (\op -> serUnop n restriction fs op unknownParityOnly valueConstraint) (allowedUnaryOps restriction),
  map (\op -> serBinop n restriction fs op unknownParityOnly valueConstraint) (allowedBinaryOps restriction)]

serIf :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Bool -> Int -> Restriction -> FoldState -> Bool -> Maybe Word64 -> m (ExpC, Bool, Int, Int, Parity)
-- bonus tasks have top-level if with a b c components of roughly equal size without nested ifs
-- "bonus" controls if we are generating this special "if"
serIf bonus n restriction_orig@(Restriction ops alz arz) fs unknownParityOnly valueConstraint = do
  -- If bonus, the options for XXX in the condition (if (and 1 XXX)) are:
  --  XXX = 0
  --  XXX = 1
  --  XXX = MainArg
  --  XXX = (not MainArg)
  --  XXX = anything whose parity is PUnknown.
  let restriction@(Restriction ops alz arz) = 
        if bonus then restriction_orig `removeOpRestriction` If_op
        else restriction_orig
  let (a_lo, a_hi) = 
        if bonus then if n < 30 then ((n-3) `div` 3,(n-3) `div` 2) else ((n-3)`div` 4,(n-3)`div`3)
        else (1,n-3)
  sizeA_ <- elements [a_lo, a_hi]
  guard $ (bonus == False || sizeA_ >= (n-2) `div` 4 - 2)
  let opsOnly = noRestriction {allowedOps = ops}
  let restrictionA = opsOnly
  let restrictionB = opsOnly
  (sizeA0, (a_, foldA, _, _, _)) <- do
    if bonus
      then msum [
         return (1, (zero, False, 64, 64, PZero))
       , return (1, (one, False, 63, 0, POne))
       , return (1, (mainArg, False, 0, 0, PMain))
       , return (2, (not_ mainArg, False, 0, 0, PNegMain))
       , do x <- (serExp' sizeA_ restrictionA fs True Nothing)
            return (sizeA_, x)
       ]
      else do x <- serExp' sizeA_ restrictionA fs False Nothing -- no constraint is possible on condition
              return (sizeA_, x)
  guard (sizeA0 >= a_lo && sizeA0 <= a_hi)
  let a = if bonus
          then and_ one a_
          else a_
  let sizeA = if bonus then sizeA0 + 2 else sizeA0
  guard $ isSimpleHead $ expr a
  if isConstExprC a
    then mzero
    else do
      let (b_lo, b_hi) = if bonus 
                         then ((n-2-sizeA) `div` 2-2,(n-2-sizeA) `div` 2+2)
                         else (1,n - 2 - sizeA)
      sizeB <- elements [b_lo..b_hi]
      guard $ (bonus == False || sizeB >= (n-2) `div` 4)
      let sizeC = n - 1 - sizeA - sizeB
      guard $ (bonus == False || sizeC >= (n-2) `div` 4)
      let aVal = cached a
      let (bValueConstraint, cValueConstraint) = 
            if isNothing64 aVal then (Nothing, Nothing)
            else if 0 == (fromMWord64 (error "too") aVal) then (valueConstraint, Nothing) else (Nothing, valueConstraint)
      (b, foldB, lzb, rzb, pyb) <- serExp' sizeB restrictionB (if foldA then ExternalFold else fs) False bValueConstraint
      -- Respecting restriction "at most alz constant-zero bits allowed in if(..) b else c":
      -- Assume b guarantees lzb left zero bits. If lzb <= alz, we're fine - no restriction on c.
      -- If lzb > alz, propagate restriction to c.
      let restrictionC = restriction {
          allowedZeroLeftBits = if lzb <= alz then 64 else alz
        , allowedZeroRightBits = if rzb <= arz then 64 else arz
        }
      (c, foldC, lzc, rzc, pyc) <- serExp' sizeC restrictionC (if (foldA || foldB) then ExternalFold else fs) False cValueConstraint
      let e = if0 a b c
      guard (isSimpleHead (expr e))
      let (lze, rze) = leftRightZerosBinop lzb rzb lzc rzc (expr e)
      let pye = parityBinop pyb pyc (expr e)
      guard (not (unknownParityOnly && pye /= PUnknown))
      return (e, foldA || foldB || foldC, lze, rze, pye)

serFold :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> Bool -> m (ExpC, Bool, Int, Int, Parity)
serFold n restriction@(Restriction ops _ _) unknownParityOnly = level $ do
  let opsOnly = noRestriction {allowedOps = ops}
  let restrictionArg = opsOnly
  let restrictionSeed = opsOnly
  let restrictionBody = restriction
  sizeArg <- elements [1..n - 4]
  sizeSeed <- elements [1..n - 3 - sizeArg]
  let sizeBody = n - 2 - sizeArg - sizeSeed
  (c, foldC, lzc, rzc, pyc) <- serExp' sizeBody restrictionBody InFoldBody False Nothing -- TODO
  guard (usesFold1Arg c || usesFold2Arg c)
  -- TODO: is this really true that for tfold we could have ridiculous bodies that do not use foldAcc?
  (a, foldA, _, _, _) <- serExp' sizeArg restrictionArg ExternalFold False Nothing -- TODO
  (b, foldB, _, _, _) <- serExp' sizeSeed restrictionSeed ExternalFold False Nothing -- TODO
  let e = fold_ a b c
  guard (isSimpleHead (expr e))
  let (lze, rze) = leftRightZerosUnop lzc rzc (expr e)
  let pye        = parityUnop pyc (expr e)
  guard (not (unknownParityOnly && pye /= PUnknown))
  return (e, True, lze, rze, pye)
    
usesFold2Arg :: ExpC -> Bool
usesFold2Arg (ExpC _ e) = u e
  where
    u Zero = False
    u One = False
    u MainArg = False
    u Fold1Arg = False
    u Fold2Arg = True
    u (If a b c) = usesFold2Arg a || usesFold2Arg b || usesFold2Arg c -- if0 branches or condition should refer to fold2Arg, otherwise they are as good as constant
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
    u (If a b c) = usesFold1Arg a || usesFold1Arg b || usesFold1Arg c -- if0 branches or condition should refer to fold1Arg, otherwise they are as good as constant
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

serUnop :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> OpName -> Bool -> Maybe Word64 -> m (ExpC, Bool, Int, Int, Parity)
serUnop n restriction@(Restriction ops alz arz) fs op unknownParityOnly valueConstraint = level $ do
  guard (not (unknownParityOnly && op == Shl1_op)) -- Shl1 always generates known (zero) parity.
  let opsOnly = noRestriction {allowedOps = ops}
  let (alz', arz') = case op of {
      Not_op -> (64, 64)
    ; Shl1_op -> (min 64 (alz+1), arz-1)
    ; Shr1_op -> (alz-1, min 64 (arz+1))
    ; Shr4_op -> (alz-4, min 64 (arz+4))
    ; Shr16_op -> (alz-16, min 64 (arz+16))
    }
  let argValueConstraint = case op of {
      Not_op -> fmap complement valueConstraint
    ; Shl1_op -> Nothing -- TODO
    ; Shr1_op -> Nothing -- TODO 
    ; Shr4_op -> Nothing -- TODO
    ; Shr16_op -> Nothing -- TODO
    }
  guard (alz' >= 0 && arz' >= 0)  -- Out of zeros budget: skip
  let restrictionArg = Restriction ops alz' arz'
  let aUnknownParityOnly = if not (unknownParityOnly) then False else case op of {
      Not_op -> unknownParityOnly
    ; Shl1_op -> False
    ; Shr1_op -> False
    ; Shr4_op -> False
    ; Shr16_op -> False
    }
  (a, foldA, lza, rza, pya) <- serExp' (n-1) restrictionArg fs aUnknownParityOnly argValueConstraint
  let e = applyUnop op a
  guard (isSimpleHead (expr e))
  let (lzu, rzu) = leftRightZerosUnop lza rza (expr e) 
  let pyu = parityUnop pya (expr e)
  guard (not (unknownParityOnly && pyu /= PUnknown))
  return (e, foldA, lzu, rzu, pyu)

serBinop :: (MonadLevel m, ?tfold :: Bool, ?cache :: Cache) => Int -> Restriction -> FoldState -> OpName -> Bool -> Maybe Word64 -> m (ExpC, Bool, Int, Int, Parity)
serBinop n restriction@(Restriction ops alz arz) fs op unknownParityOnly valueConstraint = level $ do
  let opsOnly = noRestriction {allowedOps = ops}
  let (alzA, arzA) = case op of {
      And_op -> (alz, arz)
    ; Or_op -> (64, 64)
    ; Xor_op -> (64, 64)
    ; Plus_op -> (64, 64)
    }
  let restrictionA = Restriction ops alzA arzA
  sizeA <- elements [1..n - 2]
  let sizeB = n - 1 - sizeA
  -- Normal form: first operand must be smaller in size
  if sizeA > sizeB
    then mzero
    else do
      (a, foldA, lza, rza, pya) <- serExp' sizeA restrictionA fs False Nothing -- TODO
      guard $ expr a /= Zero -- all binary ops (and, or, xor, plus) are stupid if first arg is zero
      let aValue' =  cached a
      let aValue = if isNothing64 aValue' then Nothing else Just (fromMWord64 (error "foo") aValue')
      let (alzB, arzB) = case op of {
          And_op  -> (alz, arz)
          -- if lza <= alz, we're fine: no restriction
          -- if lza > alz, responsibility of generating nonzero bits is on B: use alz.
        ; Or_op   -> (if lza <= alz then 64 else alz, if rza <= arz then 64 else arz)
        ; Xor_op  -> (if lza <= alz then 64 else alz, if rza <= arz then 64 else arz)
        ; Plus_op -> (if lza <= alz then 64 else alz, if rza <= arz then 64 else arz)
        }
      let bValueConstraint = if aValue == Nothing then Nothing
          else case op of {
            And_op  -> Nothing -- TODO
          ; Or_op   -> Nothing -- TODO
          ; Xor_op  -> liftM2 (xor) aValue valueConstraint
          ; Plus_op -> liftM2 (-) valueConstraint aValue
          }
      let restrictionB = Restriction ops alzB arzB
      guard $ case (unknownParityOnly, op, pya) of {
          (False, _, _) -> True
        ; (_, And_op, PZero) -> False
        ; (_, Or_op, POne) -> False
        ; _ -> True
        }
      let bUnknownParityOnly = if not (unknownParityOnly) then False else case (op, pya) of {
          -- Only if parity of the result fully depends on parity of b.
          (And_op, POne) -> True
        ; (Or_op, PZero) -> True
        ; (Xor_op, PZero) -> True
        ; (Plus_op, PZero) -> True
        ; _ -> False
        }
      (b, foldB, lzb, rzb, pyb) <- serExp' sizeB restrictionB (if foldA then ExternalFold else fs) bUnknownParityOnly bValueConstraint
      let e = applyBinop op a b
      guard (isSimpleHead (expr e))
      let (lze, rze) = leftRightZerosBinop lza rza lzb rzb (expr e)
      let pye = parityBinop pya pyb (expr e)
      guard (not (unknownParityOnly && pye /= PUnknown))
      return (e, foldA || foldB, lze, rze, pye)
