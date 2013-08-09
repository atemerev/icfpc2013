module Gen (generateRestricted, serProg) where

import Types
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import Data.Maybe (catMaybes)

generateRestricted :: Bool -> Int -> Restriction -> [Exp]
generateRestricted tfold n restriction = 
  if tfold
  then map (\e -> Fold MainArg Zero e) $ list (n-4) foldBodies -- |fold x 0| is 2 + 1 + 1, hence n-4
  else list n (serProg restriction)
  where
    foldBodies = do
      n <- getDepth
      (e, hasFold) <- serExp' n (S.delete Fold_op restriction) InFoldBody -- Fold should not be there, but remove it just in case
      return e

-- Generators are restricted to allowed function set
data OpName = Not_op | Shl1_op | Shr1_op | Shr4_op | Shr16_op | And_op | Or_op | Xor_op | Plus_op | If_op | Fold_op deriving (Eq, Ord)
type Restriction = S.Set OpName

allow restriction opName f =
  if opName `S.member` restriction then Just f else Nothing

allowedUnaryOps :: Restriction -> [Exp -> Exp]
allowedUnaryOps r = 
  catMaybes [ allow r op f | (op,f) <- [(Not_op,Not), (Shl1_op, Shl1), (Shr1_op, Shr1), (Shr4_op, Shr4), (Shr16_op, Shr16)]]

allowedBinaryOps :: Restriction -> [Exp -> Exp -> Exp]
allowedBinaryOps r = 
  catMaybes [ allow r op f | (op,f) <- [(And_op, And), (Or_op, Or), (Xor_op, Xor), (Plus_op, Plus)]]

allowedIf :: Restriction -> Bool
allowedIf r = If_op `S.member` r
  
allowedFold :: Restriction -> Bool
allowedFold r = Fold_op `S.member` r
      
-- 
serProg :: Monad m => Restriction -> Series m Exp
serProg restriction = decDepth (serExpression restriction)-- remove 1 level of depth for top-level lambda

serExpression :: (Monad m) => Restriction -> Series m Exp
serExpression restriction = do
  n <- getDepth
  (e, hasFold) <- serExp' n restriction NoFold
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
serExp' :: (Monad m) => Int -> Restriction -> FoldState -> Series m (Exp, Bool)
serExp' 1 _ InFoldBody = oneof $ map (\x -> return (x, False)) [Zero, One, MainArg, Fold1Arg, Fold2Arg]
serExp' 1 _ _ = oneof $ map (\x -> return (x, False)) [Zero, One, MainArg]
serExp' n restriction fs = oneof $ concat [
  if (n >= 4 && allowedIf restriction) then [serIf n restriction fs] else [],
  if (n >= 5 && fs == NoFold && allowedFold restriction) then [serFold n (S.delete Fold_op restriction)] else [],
  if n >= 2 then map (\op -> serUnop n restriction fs op) (allowedUnaryOps restriction) else [], 
  if n >= 3 then map (\op -> serBinop n restriction fs op) (allowedBinaryOps restriction) else []]

serIf :: (Monad m) => Int -> Restriction -> FoldState -> Series m (Exp, Bool)
serIf n restriction fs = do
  sizeA <- elements [1..n - 3]
  sizeB <- elements [1..n - 2 - sizeA]
  let sizeC = n - 1 - sizeA - sizeB
  (a, foldA) <- serExp' sizeA restriction fs
  (b, foldB) <- serExp' sizeB restriction (if foldA then ExternalFold else fs)
  (c, foldC) <- serExp' sizeC restriction (if (foldA || foldB) then ExternalFold else fs)
  return (If a b c, foldA || foldB || foldC)

serFold :: (Monad m) => Int -> Restriction -> Series m (Exp, Bool)
serFold n restriction = do
  sizeArg <- elements [1..n - 4]
  sizeSeed <- elements [1..n - 3 - sizeArg]
  let sizeBody = n - 2 - sizeArg - sizeSeed
  (a, foldA) <- serExp' sizeArg restriction ExternalFold
  (b, foldB) <- serExp' sizeSeed restriction ExternalFold
  (c, foldC) <- serExp' sizeBody restriction InFoldBody
  return (Fold a b c, True)

serUnop :: (Monad m) => Int -> Restriction -> FoldState -> (Exp -> Exp) -> Series m (Exp, Bool)
serUnop n restriction fs op = do
  (a, foldA) <- serExp' (n-1) restriction fs
  return (op a, foldA)

serBinop :: (Monad m) => Int -> Restriction -> FoldState -> (Exp -> Exp -> Exp) -> Series m (Exp, Bool)
serBinop n restriction fs op = do
  sizeA <- elements [1..n - 2]
  let sizeB = n - 1 - sizeA
  (a, foldA) <- serExp' sizeA restriction fs
  (b, foldB) <- serExp' sizeB restriction (if foldA then ExternalFold else fs)
  return (op a b, foldA || foldB)
