{-# LANGUAGE GADTs, DeriveDataTypeable, BangPatterns, ImplicitParams, MagicHash #-}
module Types
  ( Exp(..)
  , ExpC(..)
  , Word64
  , MWord64
  , nothing64
  , fromMWord64
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

-- Packed "Maybe Word64"
data MWord64 = MWord64 {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
  deriving (Show, Data, Typeable)

just64 :: Word64 -> MWord64
just64 !x = MWord64 1 x

nothing64 :: MWord64
nothing64 = MWord64 0 0

isNothing64 :: MWord64 -> Bool
isNothing64 (MWord64 0 x) = True
isNothing64 _ = False

unsafeValue64 :: MWord64 -> Word64
unsafeValue64 (MWord64 _ x) = x

fromMWord64 :: Word64 -> MWord64 -> Word64
fromMWord64 def x = if isNothing64 x then def else unsafeValue64 x

-- We use this function only when it's cheaper to apply f than to check b :)
map64 :: (Word64 -> Word64) -> MWord64 -> MWord64
map64 f x = if isNothing64 x then nothing64 else just64 (f (unsafeValue64 x))

zip64 :: (Word64 -> Word64 -> Word64) -> MWord64 -> MWord64 -> MWord64
zip64 f x y = if isNothing64 x || isNothing64 y
              then nothing64
              else just64 (f (unsafeValue64 x) (unsafeValue64 y))

data ExpC = ExpC {cached :: {-# UNPACK #-} !MWord64, expr :: Exp} deriving (Show, Data, Typeable)
instance Eq ExpC where
  (ExpC _ a) == (ExpC _ b) = a == b
instance Ord ExpC where
  compare (ExpC _ a) (ExpC _ b) = compare a b

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

evalOnSeed :: MWord64 -> MWord64 -> Exp -> MWord64
evalOnSeed fold1 fold2 e = {-# SCC "evalOnSeed" #-}
  case e of
    Zero -> just64 0
    One -> just64 1
    MainArg -> just64 seed
    Fold1Arg -> fold1
    Fold2Arg -> fold2
    If a b c ->
      case ev fold1 fold2 a of {
        x | isNothing64 x -> nothing64
          | unsafeValue64 x == 0 -> ev fold1 fold2 b
          | otherwise            -> ev fold1 fold2 c
      }

    Fold a b c ->
      let
        evalFn :: Word64 -> Word64 -> ExpC -> Word64
        evalFn !f1Arg !f2Arg body = case ev (just64 f1Arg) (just64 f2Arg) body of {
          x | isNothing64 x -> error $ "Failed evaluation within fold: (f1,f2,body) = " ++ show (f1Arg, f2Arg, body)
            | otherwise -> unsafeValue64 x
        }
      in case (ev fold1 fold2 a, ev fold1 fold2 b) of {
           (x, y) | isNothing64 x || isNothing64 y -> nothing64
                  | otherwise                      -> just64 $ foldImpl evalFn (unsafeValue64 x) (unsafeValue64 y) c
         }
    Not a   -> map64 complement (ev fold1 fold2 a)
    Shl1 a  -> map64 (\x -> x `shiftL` 1) (ev fold1 fold2 a)
    Shr1 a  -> map64 (\x -> x `shiftR` 1) (ev fold1 fold2 a)
    Shr4 a  -> map64 (\x -> x `shiftR` 4) (ev fold1 fold2 a)
    Shr16 a -> map64 (\x -> x `shiftR` 16) (ev fold1 fold2 a)
    And a b -> zip64 (.&.) (ev fold1 fold2 a) (ev fold1 fold2 b)
    Or a b ->  zip64 (.|.) (ev fold1 fold2 a) (ev fold1 fold2 b)
    Xor a b -> zip64 (xor) (ev fold1 fold2 a) (ev fold1 fold2 b)
    Plus a b -> zip64 (+) (ev fold1 fold2 a) (ev fold1 fold2 b)
  where
    ev :: MWord64 -> MWord64 -> ExpC -> MWord64
    ev !fold1 !fold2 x = {-# SCC "ev" #-} case cached x of {
        c | not (isNothing64 c) -> c
        -- if x's value was unknown but fold args are now in scope, re-evaluate
        -- the tree
          | otherwise -> case (fold1, fold2) of {
              (a, b) | isNothing64 a || isNothing64 b -> nothing64
                     | otherwise -> {-# SCC "reevaluate" #-} evalOnSeed fold1 fold2 (expr x)
            }
      }

cache e = ExpC (evalOnSeed nothing64 nothing64 e) e
zero = ExpC (just64 0) Zero
one  = ExpC (just64 1) One
mainArg = ExpC (just64 seed) MainArg
fold1Arg = ExpC nothing64 Fold1Arg
fold2Arg = ExpC nothing64 Fold2Arg
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

