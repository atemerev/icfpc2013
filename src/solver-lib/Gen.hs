module Gen (generateAll, serProg) where

import Types
import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad

generateAll :: Int -> [Exp]
generateAll n = list n serProg

serProg:: Monad m => Series m Exp
serProg = decDepth serExp -- remove 1 level of depth for top-level lambda

serExp :: (Monad m) => Series m Exp
serExp = do
  n <- getDepth
  (e, hasFold) <- serExp' n NoFold
  return e

data FoldState = NoFold -- Allowed to generate unrestricted expression except references to fold args
               | ExternalFold -- Not allowed to generate folds or references to fold args
               | InFoldBody -- Allowed to generate references to fold args but not folds
               deriving (Eq, Show)

-- Names of the operations that we are allowed to use while generating
type Restriction = [Exp]

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
  (a, foldA) <- serExp' sizeArg ExternalFold
  (b, foldB) <- serExp' sizeSeed ExternalFold
  (c, foldC) <- serExp' sizeBody InFoldBody
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

-- names of the ops for limiting sets of possible ops

-- data RestrictedExp = RExp { exp::Exp, foldState::FoldState, ops::[OpName] }
