{-# LANGUAGE ImplicitParams #-}
module ProgramCounting (
  inFoldExprCount,
  topLevelNoFoldCount,
  topLevelCount,
  tfoldCount
) where
import System.Environment (getArgs)

import Data.Array
import Data.Array.IArray (amap)
import Data.List (sort, nub)
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

ufC = listArray (1,size) [ uf i | i <- [1..size]]
afsC = listArray (1,size) [ afs i | i <- [1..size]]
tfC = listArray (1,size) [ tf i | i <- [1..size]]
afC = listArray (1,size) [ af i | i <- [1..size]]

type D = Int -- depth
data Tag = UF D | AFS D | AF D | TF D
         | C0 | C1 | X | Y | Z
         | Not Tag | Shl1 Tag | Shr1 Tag | Shr4 Tag | Shr16 Tag
         | And Tag Tag | Or Tag Tag | Xor Tag Tag | Plus Tag Tag
         | If0 Tag Tag Tag | Fold Tag Tag Tag
         deriving (Show, Eq, Ord)

isLeaf :: Tag -> Bool
isLeaf tag = tag `elem` [C0,C1,X,Y,Z]

data Context = Ctx { allowedOp1 :: [AllowedOp1]
                   , allowedOp2 :: [AllowedOp2]
                   , isIfAllowed :: !Bool
                   , isFoldAllowed :: !Bool
                   , inFoldExprExpands :: Array Int [Tag]
                   , topLevelNoFoldExpands :: Array Int [Tag]
                   , topLevelExpands :: Array Int [Tag]
                   , inTFoldExprExpands :: Array Int [Tag]
                   , inFoldExprCounts :: Array Int Integer
                   , topLevelNoFoldCounts :: Array Int Integer
                   , topLevelCounts :: Array Int Integer
                   , inTFoldExprCounts :: Array Int Integer
                   }

type AllowedOp1 = Tag -> Tag
type AllowedOp2 = Tag -> Tag -> Tag

defaultAllowedOp1 = [Not, Shl1, Shr1, Shr4, Shr16]
defaultAllowedOp2 = [And, Or, Xor, Plus]

defaultContext :: Context
defaultContext = newContext defaultAllowedOp1 defaultAllowedOp2 True True

newContext :: [AllowedOp1] -> [AllowedOp2] -> Bool -> Bool -> Context
newContext allowedOp1 allowedOp2 isIfAllowed isFoldAllowed =
  let ctx = (let ?ctx = ctx in Ctx
                 { allowedOp1 = allowedOp1
                 , allowedOp2 = allowedOp2
                 , isIfAllowed = isIfAllowed
                 , isFoldAllowed = isFoldAllowed
                 , inFoldExprExpands = listArray (1,size) [ expand (UF n) | n <- [1..size] ]
                 , topLevelNoFoldExpands = listArray (1,size) [ expand (AFS n) | n <- [1..size] ]
                 , topLevelExpands = listArray (1,size) [ expand (AF n) | n <- [1..size] ]
                 , inTFoldExprExpands = listArray (1,size) [ expand (TF n) | n <- [1..size] ]
                 , inFoldExprCounts = amap (\expands -> sum $ map sizeTag expands) (inFoldExprExpands ctx)
                 , topLevelNoFoldCounts = amap (\expands -> sum $ map sizeTag expands) (topLevelNoFoldExpands ctx)
                 , topLevelCounts = amap (\expands -> sum $ map sizeTag expands) (topLevelExpands ctx)
                 , inTFoldExprCounts = amap (\expands -> sum $ map sizeTag expands) (inTFoldExprExpands ctx)
                 }) in ctx

expand :: (?ctx :: Context) => Tag -> [Tag]
expand (UF 1) = [C0, C1, X, Y, Z]
expand (UF n) = concat
  [ [ op1 (UF $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (UF i) (UF $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (UF i) (UF j) (UF $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]

expand (AFS 1) = [C0, C1, X]
expand (AFS n) = concat
  [ [ op1 (AFS $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (AFS i) (AFS $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (AFS i) (AFS j) (AFS $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]

expand (AF 1) = [C0, C1, X]
expand (AF n) | not (isFoldAllowed ?ctx) = expand (AFS n)
              | otherwise = concat
  [ [ op1 (AF $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (AF i) (AFS $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ op2 (AFS i) (AF $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (af1 i) (af2 j) (af3 $ n-1-i-j) | isIfAllowed ?ctx,
                                           (af1,af2,af3) <- [(AF,AFS,AFS), (AFS,AF,AFS), (AFS,AFS,AF)],
                                           i <- [1..n-3], j <- [1..n-2-i] ]
  , [ Fold (AFS i) (AFS j) (UF $ n-2-i-j) | i <- [1..n-4], j <- [1..n-3-i] ]
  ]

expand (TF 1) = [C0, C1, Y, Z]   -- can't use X!
expand (TF n) = concat
  [ [ op1 (TF $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (TF i) (TF $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (TF i) (TF j) (TF $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]
expand C0 = [C0]
expand C1 = [C0]
expand X = [X]
expand Y = [Y]
expand Z = [Z]
expand (Not tag) = map Not (expand tag)
expand (Shl1 tag) = map Shl1 (expand tag)
expand (Shr1 tag) = map Shr1 (expand tag)
expand (Shr4 tag) = map Shr4 (expand tag)
expand (Shr16 tag) = map Shr16 (expand tag)
expand (And tag1 tag2) = [ And a b | a <- expand tag1, b <- expand tag2 ]
expand (Or tag1 tag2) = [ Or a b | a <- expand tag1, b <- expand tag2 ]
expand (Xor tag1 tag2) = [ Xor a b | a <- expand tag1, b <- expand tag2 ]
expand (Plus tag1 tag2) = [ Plus a b | a <- expand tag1, b <- expand tag2 ]
expand (If0 tag1 tag2 tag3) = [ If0 a b c | a <- expand tag1, b <- expand tag2, c <- expand tag3 ]
expand (Fold tag1 tag2 tag3) = [ Fold a b c | a <- expand tag1, b <- expand tag2, c <- expand tag3 ]

sizeTag :: (?ctx :: Context) => Tag -> Integer
sizeTag C0 = 1
sizeTag C1 = 1
sizeTag X = 1
sizeTag Y = 1
sizeTag Z = 1
sizeTag (Not tag) = sizeTag tag
sizeTag (Shl1 tag) = sizeTag tag
sizeTag (Shr1 tag) = sizeTag tag
sizeTag (Shr4 tag) = sizeTag tag
sizeTag (Shr16 tag) = sizeTag tag
sizeTag (And tag1 tag2) = sizeTag tag1 * sizeTag tag2
sizeTag (Or tag1 tag2) = sizeTag tag1 * sizeTag tag2
sizeTag (Xor tag1 tag2) = sizeTag tag1 * sizeTag tag2
sizeTag (Plus tag1 tag2) = sizeTag tag1 * sizeTag tag2
sizeTag (If0 tag1 tag2 tag3) = sizeTag tag1 * sizeTag tag2 * sizeTag tag3
sizeTag (Fold tag1 tag2 tag3) = sizeTag tag1 * sizeTag tag2 * sizeTag tag3
sizeTag (UF n) = (inFoldExprCounts ?ctx) ! n
sizeTag (AFS n) = (topLevelNoFoldCounts ?ctx) ! n
sizeTag (AF n) = (topLevelCounts ?ctx) ! n
sizeTag (TF n) = (inTFoldExprCounts ?ctx) ! n

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
       5 * ufC ! (m-1) +                                                                              -- op1 uf
       4 * sum [ ufC ! i * ufC ! j | i <- [1..m-2], let j = m-1-i ] +                                 -- op2 uf uf
       sum [ ufC ! i * ufC ! j * ufC ! k | i <- [1..m-3], j <- [1..m-2-i], let k = m-1-i-j ]          -- if0 uf uf uf

main = do
  putStrLn "size,inFoldExprCount,topLevelNoFoldCount,topLevelCount,tfoldCount"
  forM_ [1..size] $ \s ->
    printf "%d,%d,%d,%d,%d\n" s (ufC ! s) (afsC ! s) (afC ! s) (tfoldCount !s)
