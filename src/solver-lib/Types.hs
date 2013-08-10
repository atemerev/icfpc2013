{-# LANGUAGE GADTs, DeriveDataTypeable, BangPatterns, MagicHash #-}
module Types
  ( Exp(..)
  , Word64
  , eval
  , isConstExpr
  , progSize
  , expSize
  )
  where

import GHC.Prim
import GHC.Types
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
eval main fold1 fold2 e = case (fromIntegral main, fromIntegral fold1, fromIntegral fold2) of
  (W# main#, W# fold1#, W# fold2#) -> fromIntegral $ W# (eval' main# fold1# fold2# e)

eval' :: Word# -> Word# -> Word# -> Exp -> Word#
eval' main fold1 fold2 e = case e of
    Zero -> 0##
    One -> 1##
    MainArg -> main
    Fold1Arg -> fold1
    Fold2Arg -> fold2
    If c t f -> if word2Int# (eval' main fold1 fold2 c) ==# 0#
                then eval' main fold1 fold2 t
                else eval' main fold1 fold2 f
    Not e -> not# (eval' main fold1 fold2 e)
    Shl1 e -> uncheckedShiftL# (eval' main fold1 fold2 e) 1#
    Shr1 e -> uncheckedShiftRL# (eval' main fold1 fold2 e) 1#
    Shr4 e -> uncheckedShiftRL# (eval' main fold1 fold2 e) 4#
    Shr16 e -> uncheckedShiftRL# (eval' main fold1 fold2 e) 16#
    And e1 e2 -> eval' main fold1 fold2 e1 `and#` eval' main fold1 fold2 e2
    Or e1 e2 -> eval' main fold1 fold2 e1 `or#` eval' main fold1 fold2 e2
    Xor e1 e2 -> eval' main fold1 fold2 e1 `xor#` eval' main fold1 fold2 e2
    Plus e1 e2 -> eval' main fold1 fold2 e1 `plusWord#` eval' main fold1 fold2 e2
    Fold arg seed body -> {-# SCC "foldImpl" #-} foldImpl (eval' main fold1 fold2 arg) (eval' main fold1 fold2 seed) body
  where foldImpl x seed body = op x0 (op x1 (op x2 (op x3 (op x4 (op x5 (op x6 (op x7 seed)))))))
          where
            op :: Word# -> Word# -> Word#
            op a b = eval' main a b body
            -- x7 is least significant, x0 most significant
            x7654 = narrow32Word# x
            x3210 = x `uncheckedShiftRL#` 32#
            x76 = narrow16Word# x7654
            x54 = x7654 `uncheckedShiftRL#` 16#
            x32 = narrow16Word# x3210
            x10 = x3210 `uncheckedShiftRL#` 16#
            x7 = narrow8Word# x76
            x6 = x76 `uncheckedShiftRL#` 8#
            x5 = narrow8Word# x54
            x4 = x54 `uncheckedShiftRL#` 8#
            x3 = narrow8Word# x32
            x2 = x32 `uncheckedShiftRL#` 8#
            x1 = narrow8Word# x10
            x0 = x10 `uncheckedShiftRL#` 8#

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

