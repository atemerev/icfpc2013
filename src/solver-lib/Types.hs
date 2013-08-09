{-# LANGUAGE GADTs, DeriveDataTypeable #-}
module Types
  ( Exp(..)
  , Word64
  , eval
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

