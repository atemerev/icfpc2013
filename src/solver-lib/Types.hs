{-# LANGUAGE GADTs, DeriveDataTypeable, BangPatterns, ImplicitParams #-}
module Types
  ( Exp(..)
  , ExpC(..)
  , Word64
  , eval
  , isConstExpr
  , isConstExprC
  , progSize
  , expSize
  , expCSize
  , zero, one, mainArg, fold1Arg, fold2Arg, if0, fold_, not_, shl1, shr1, shr4, shr16, and_ , or_ , xor_, plus
  )
  where

import Data.Word
import Data.Bits
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Maybe
import Control.Applicative
import Control.Monad
import RandomBV

-- Expression that caches result of evaluation on the first bitvector that we would test on
seed = head bvs

data ExpC = ExpC {cached :: !(Maybe Word64), expr :: Exp} deriving (Eq, Ord, Show, Data, Typeable)
data Exp =
    Zero
  | One
  | MainArg
  | Fold1Arg
  | Fold2Arg
  | If { ifCond :: ExpC, ifTrue :: ExpC, ifFalse :: ExpC }
  | Fold { foldArg :: ExpC, foldSeed :: ExpC, foldBody :: ExpC }
  | Not ExpC
  | Shl1 ExpC
  | Shr1 ExpC
  | Shr4 ExpC
  | Shr16 ExpC
  | And ExpC ExpC
  | Or ExpC ExpC
  | Xor ExpC ExpC
  | Plus ExpC ExpC
  deriving (Eq, Ord, Show, Data, Typeable)

isConstExprC (ExpC _ e) = isConstExpr e

isConstExpr Zero = True
isConstExpr One = True
isConstExpr MainArg = False
isConstExpr Fold1Arg = False
isConstExpr Fold2Arg = False
isConstExpr (If a b c) = isConstExprC a && isConstExprC b && isConstExprC c
isConstExpr (Fold a b c) = isConstExprC a && isConstExprC b && isConstExprC c
isConstExpr (Not a) = isConstExprC a
isConstExpr (Shl1 a) = isConstExprC a
isConstExpr (Shr1 a) = isConstExprC a
isConstExpr (Shr4 a) = isConstExprC a
isConstExpr (Shr16 a) = isConstExprC a
isConstExpr (And a b) = isConstExprC a && isConstExprC b
isConstExpr (Or a b) = isConstExprC a && isConstExprC b
isConstExpr (Xor a b) = isConstExprC a && isConstExprC b
isConstExpr (Plus a b) = isConstExprC a && isConstExprC b

eval :: Word64 -> Word64 -> Word64 -> ExpC -> Word64
eval main fold1 fold2 e =
  case expr e of
    Zero -> 0
    One -> 1
    MainArg -> main
    Fold1Arg -> fold1
    Fold2Arg -> fold2
    If c t f -> if eval main fold1 fold2 c == 0
                then eval main fold1 fold2 t
                else eval main fold1 fold2 f
    Not e -> complement (eval main fold1 fold2 e)
    Shl1 e -> shiftL (eval main fold1 fold2 e) 1
    Shr1 e -> shiftR (eval main fold1 fold2 e) 1
    Shr4 e -> shiftR (eval main fold1 fold2 e) 4
    Shr16 e -> shiftR (eval main fold1 fold2 e) 16
    And e1 e2 -> eval main fold1 fold2 e1 .&. eval main fold1 fold2 e2
    Or e1 e2 -> eval main fold1 fold2 e1 .|. eval main fold1 fold2 e2
    Xor e1 e2 -> xor (eval main fold1 fold2 e1) (eval main fold1 fold2 e2)
    Plus e1 e2 -> eval main fold1 fold2 e1 + eval main fold1 fold2 e2
    Fold arg seed body -> foldImpl (eval main) (eval main fold1 fold2 arg) (eval main fold1 fold2 seed) body

foldImpl
  :: (Word64 -> Word64 -> ExpC -> Word64)
    -- ^ function to use for evaluation of fold body. We it may be plain
    -- 'eval', or its cached version. The arguments are fold1Arg, fold2Arg,
    -- and body.
  -> Word64 -- ^ fold1Arg
  -> Word64 -- ^ fold2Arg
  -> ExpC   -- ^ body
  -> Word64
foldImpl eval !x !seed body = op x0 (op x1 (op x2 (op x3 (op x4 (op x5 (op x6 (op x7 seed)))))))
  where
    op !a !b = eval a b body
    {-# INLINE op #-}
    -- x7 is least significant, x0 most significant
    (!x7', !x7) = x `divMod` 256
    (!x6', !x6) = x7' `divMod` 256
    (!x5', !x5) = x6' `divMod` 256
    (!x4', !x4) = x5' `divMod` 256
    (!x3', !x3) = x4' `divMod` 256
    (!x2', !x2) = x3' `divMod` 256
    (!x1', !x1) = x2' `divMod` 256
    (!x0', !x0) = x1' `divMod` 256

evalOnSeed :: (?foldArgs :: Maybe (Word64, Word64)) => Exp -> Maybe Word64
evalOnSeed e =
  case e of
    Zero -> Just 0
    One -> Just 1
    MainArg -> Just seed
    Fold1Arg -> fst <$> ?foldArgs
    Fold2Arg -> snd <$> ?foldArgs
    If a b c ->
      case ev a of
        Just 0 -> ev b
        Just _ -> ev c
        Nothing -> Nothing
    Fold a b c ->
      let
        evalFn :: Word64 -> Word64 -> ExpC -> Word64
        evalFn f1Arg f2Arg body =
          let ?foldArgs = Just (f1Arg, f2Arg)
          in fromMaybe (error "evalOnSeed.Fold") $ ev body
      in foldImpl evalFn <$> ev a <*> ev b <*> pure c
    Not a   -> complement <$> ev a
    Shl1 a  -> shiftL <$> ev a <*> pure 1
    Shr1 a  -> shiftR <$> ev a <*> pure 1
    Shr4 a  -> shiftR <$> ev a <*> pure 4
    Shr16 a -> shiftR <$> ev a <*> pure 16
    And a b -> (.&.) <$> ev a <*> ev b
    Or a b ->  (.|.) <$> ev a <*> ev b
    Xor a b -> xor <$> ev a <*> ev b
    Plus a b -> (+) <$> ev a <*> ev b
  where
    ev :: (?foldArgs :: Maybe (Word64, Word64)) => ExpC -> Maybe Word64
    ev x =
      mplus (cached x) $ -- if x's value is known, just use it
      (if isJust ?foldArgs
        then
          -- if x's value was unknown but fold args are now in scope, re-evaluate
          -- the tree
          evalOnSeed $ expr x
        else
          -- otherwise, give up
          Nothing
      )

cache e = 
  let ?foldArgs = Nothing in 
  ExpC (evalOnSeed e) e
zero = ExpC (Just 0) Zero
one  = ExpC (Just 1) One
mainArg = ExpC (Just seed) MainArg
fold1Arg = ExpC Nothing Fold1Arg
fold2Arg = ExpC Nothing Fold2Arg
if0 a b c = cache (If a b c)
fold_ a b c = cache (Fold a b c)
not_ a   = cache (Not a)
shl1 a  = cache (Shl1 a)
shr1 a  = cache (Shr1 a)
shr4 a  = cache (Shr4 a)
shr16 a = cache (Shr16 a)
and_ a b = cache (And a b)
or_ a b = cache (Or a b)
xor_ a b = cache (Xor a b)
plus a b = cache (Plus a b)

progSize :: Exp -> Int
progSize e = expSize e + 1

expCSize ec = expSize (expr ec)

expSize :: Exp -> Int
expSize Zero = 1
expSize One = 1
expSize MainArg = 1
expSize Fold1Arg = 1
expSize Fold2Arg = 1
expSize (If a b c) = 1 + expCSize a + expCSize b + expCSize c
expSize (Fold a b c) = 2 + expCSize a + expCSize b + expCSize c
expSize (Not a) = 1 + expCSize a
expSize (Shl1 a) = 1 + expCSize a
expSize (Shr1 a) = 1 + expCSize a
expSize (Shr4 a) = 1 + expCSize a
expSize (Shr16 a) = 1 + expCSize a
expSize (And a b) = 1 + expCSize a + expCSize b
expSize (Or a b) = 1 + expCSize a + expCSize b
expSize (Xor a b) = 1 + expCSize a + expCSize b
expSize (Plus a b) = 1 + expCSize a + expCSize b

