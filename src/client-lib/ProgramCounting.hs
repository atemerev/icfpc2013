module ProgramCounting (
  inFoldExprCount,
  topLevelNoFoldCount,
  topLevelCount,
  tfoldCount
) where
import System.Environment (getArgs)

import Data.Array
import Control.Monad
import Text.Printf

size = 30

inFoldExprCount, topLevelNoFoldCount, topLevelCount, tfoldCount :: Array Int Integer

inFoldExprCount = ufC
topLevelNoFoldCount = afsC
topLevelCount = afC
-- tfold is `(lambda (x) (fold x 0 (lambda (x y) e))`, and the only changing part is `e`, so tfold's size is the size of `e` + 5
tfoldCount = tfC


-------------------------------------------------------------------------------

uf, afs, af, tf :: Int -> Integer
ufC, afsC, afC, tfC :: Array Int Integer

afsC = array (1,size) [(i, afs i) | i <- [1..size]]
ufC = array (1,size) [(i, uf i) | i <- [1..size]]
tfC = array (1,size) [(i, tf i) | i <- [1..size]]
afC = array (1,size) [(i, af i) | i <- [1..size]]

type D = Int -- depth
data Tag = AF D | AFS D | UF D| TF D | C0 | C1 | X | Y | Z
         | Not Tag | Shl1 Tag | Shr1 Tag | Shr4 Tag | Shr16 Tag
         | And Tag Tag | Or Tag Tag | Xor Tag Tag | Plus Tag Tag
         | If0 Tag Tag Tag | Fold Tag Tag Tag
         deriving (Show, Eq)

isLeaf :: Tag -> Bool
isLeaf tag = tag `elem` [C0,C1,X,Y,Z]

type AllowedOp1 = Tag -> Tag
type AllowedOp2 = Tag -> Tag -> Tag

defaultAllowedOp1 = [Not, Shl1, Shr1, Shr4, Shr16]
defaultAllowedOp2 = [And, Or, Xor, Plus]

expand :: Tag -> [AllowedOp1] -> [AllowedOp2] -> Bool -> Bool -> [Tag]
expand (AF 1) _ _ _ _ = [C0, C1, X]
expand (AF n) allowedOp1 allowedOp2 isIfAllowed False = expand (AFS n) allowedOp1 allowedOp2 isIfAllowed False
expand (AF n) allowedOp1 allowedOp2 isIfAllowed True = concat
  [ [ op1 (AFS $ n-1) | op1 <- allowedOp1 ]
  , [ op2 (AF i) (AFS $ n-1-i) | op2 <- allowedOp2, i <- [1..n-2] ]
  , [ op2 (AFS i) (AF $ n-1-i) | op2 <- allowedOp2, i <- [1..n-2] ]
  , [ If0 (af1 i) (af2 j) (af3 $ n-1-i-j) | isIfAllowed,
                                           (af1,af2,af3) <- [(AF,AFS,AFS), (AFS,AF,AFS), (AFS,AFS,AF)],
                                           i <- [1..n-3], j <- [1..n-2-i] ]
  , [ Fold (AFS i) (AFS j) (UF $ n-2-i-j) | i <- [1..n-4], j <- [1..n-3-i] ]
  ]

expand (AFS 1) _ _ _ _ = [C0, C1, X]
expand (AFS n) allowedOp1 allowedOp2 isIfAllowed _ = concat
  [ [ op1 (AFS $ n-1) | op1 <- allowedOp1 ]
  , [ op2 (AFS i) (AFS $ n-1-i) | op2 <- allowedOp2, i <- [1..n-2] ]
  , [ If0 (AFS i) (AFS j) (AFS $ n-1-i-j) | isIfAllowed, i <- [1..n-3], j <- [1..n-2-i] ]
  ]

expand (UF 1) _ _ _ _ = [C0, C1, X, Y, Z]
expand (UF n) allowedOp1 allowedOp2 isIfAllowed _ = concat
  [ [ op1 (UF $ n-1) | op1 <- allowedOp1 ]
  , [ op2 (UF i) (UF $ n-1-i) | op2 <- allowedOp2, i <- [1..n-2] ]
  , [ If0 (UF i) (UF j) (UF $ n-1-i-j) | isIfAllowed, i <- [1..n-3], j <- [1..n-2-i] ]
  ]


-- expression under fold, can use everything except fold
uf 1 = 5                                                                                              -- 0 1 x y z
uf n = 5 * ufC ! (n-1) +                                                                              -- op1 uf
       4 * sum [ ufC ! i * ufC ! j | i <- [1..n-2], let j = n-1-i ] +                                 -- op2 uf uf
       sum [ ufC ! i * ufC ! j * ufC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ]          -- if0 uf uf uf

-- expression that can't contain fold, can't contain fold variables
afs 1 = 3                                                                                             -- 0 1 x
afs n = 5 * afsC ! (n-1) +                                                                            -- op1 afs
        4 * sum [ afsC ! i * afsC ! j | i <- [1..n-2], let j = n-1-i ] +                              -- op2 afs afs
        sum [ afsC ! i * afsC ! j * afsC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ]      -- if0 afs afs afs

-- expression above fold, can contain 1 fold
af 1 = 3                                                                                              -- 0 1 x
af n = 5 * afC ! (n-1) +                                                                              -- op1 af
       4 * 2 * sum [ afC ! i * afsC ! j | i <- [1..n-2], let j = n-1-i ] +                            -- op2 af afs, op2 afs af
       3 * sum [ afC ! i * afsC ! j * afsC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ] +  -- if0 af afs afs, if0 afs af afs, if0 afs afs af
       sum [ afsC ! i * afsC ! j * ufC ! k | i <- [1..n-4], j <- [1..n-3-i], let k = n-2-i-j ]        -- fold afs afs uf

-- expression in tfold, can't contain x, can't contain fold, size is minimum 6
tf n | n <= 5 = 0
     | n == 6 = 4                                                                                     -- 0 1 y z
     | otherwise = let m = n-5 in
       5 * ufC ! (m-1) +                                                                              -- op1 tf
       4 * sum [ ufC ! i * ufC ! j | i <- [1..m-2], let j = m-1-i ] +                                 -- op2 tf tf
       sum [ ufC ! i * ufC ! j * ufC ! k | i <- [1..m-3], j <- [1..m-2-i], let k = m-1-i-j ]          -- if0 tf tf tf

main = do
  putStrLn "size,inFoldExprCount,topLevelNoFoldCount,topLevelCount,tfoldCount"
  forM_ [1..size] $ \s ->
    printf "%d,%d,%d,%d,%d\n" s (ufC ! s) (afsC ! s) (afC ! s) (tfoldCount !s)
