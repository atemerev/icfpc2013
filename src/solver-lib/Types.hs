{-# LANGUAGE GADTs, DeriveDataTypeable, BangPatterns #-}
module Types
  ( Exp(..)
  , Word64
  , eval
  , isConstExpr
  , progSize
  , expSize
  )
  where

import Data.Word
import Data.Bits
import Data.Data (Data)
import Data.Typeable (Typeable)

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
  deriving (Eq, Ord, Show, Data, Typeable)

isConstExpr Zero = True
isConstExpr One = True
isConstExpr MainArg = False
isConstExpr Fold1Arg = False
isConstExpr Fold2Arg = False
isConstExpr (If a b c) = isConstExpr a && isConstExpr b && isConstExpr c
isConstExpr (Fold a b c) = isConstExpr a && isConstExpr b && isConstExpr c
isConstExpr (Not a) = isConstExpr a
isConstExpr (Shl1 a) = isConstExpr a
isConstExpr (Shr1 a) = isConstExpr a
isConstExpr (Shr4 a) = isConstExpr a
isConstExpr (Shr16 a) = isConstExpr a
isConstExpr (And a b) = isConstExpr a && isConstExpr b
isConstExpr (Or a b) = isConstExpr a && isConstExpr b
isConstExpr (Xor a b) = isConstExpr a && isConstExpr b
isConstExpr (Plus a b) = isConstExpr a && isConstExpr b

eval :: Word64 -> Word64 -> Word64 -> Exp -> Word64
eval main fold1 fold2 e = case e of
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
    Fold arg seed body -> foldImpl (eval main fold1 fold2 arg) (eval main fold1 fold2 seed) body
  where foldImpl !x !seed body = op x0 (op x1 (op x2 (op x3 (op x4 (op x5 (op x6 (op x7 seed)))))))
          where
            op !a !b = eval main a b body
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

