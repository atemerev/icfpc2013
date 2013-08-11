{-# LANGUAGE ImplicitParams #-}
module ProgramCounting (
) where
--import System.Environment (getArgs)

import Data.Array
import Data.Array.IArray (amap)
import Data.List (sort, nub)
import Control.Monad
import Text.Printf

size = 30

--inFoldExprCount, topLevelNoFoldCount, topLevelCount, tfoldCount :: Array Int Integer

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
                 , inFoldExprCounts = amap (\expands -> sum $ map countTag expands) (inFoldExprExpands ctx)
                 , topLevelNoFoldCounts = amap (\expands -> sum $ map countTag expands) (topLevelNoFoldExpands ctx)
                 , topLevelCounts = amap (\expands -> sum $ map countTag expands) (topLevelExpands ctx)
                 , inTFoldExprCounts = amap (\expands -> sum $ map countTag expands) (inTFoldExprExpands ctx)
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
  , [ op2 (AF i) (AF $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (AF i) (AF j) (AF $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
{-
  , [ op2 (AF i) (AFS $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ op2 (AFS i) (AF $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (af1 i) (af2 j) (af3 $ n-1-i-j) | isIfAllowed ?ctx,
                                           (af1,af2,af3) <- [(AF,AFS,AFS), (AFS,AF,AFS), (AFS,AFS,AF)],
                                           i <- [1..n-3], j <- [1..n-2-i] ]
-}
  , [ Fold (AFS i) (AFS j) (UF $ n-2-i-j) | i <- [1..n-4], j <- [1..n-3-i] ]
  ]

expand (TF 1) = [C0, C1, Y, Z]   -- can't use X!
expand (TF n) = concat
  [ [ op1 (TF $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (TF i) (TF $ n-1-i) | op2 <- allowedOp2 ?ctx, i <- [1..n-2] ]
  , [ If0 (TF i) (TF j) (TF $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]
expand C0 = [C0]
expand C1 = [C1]
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

countTag :: (?ctx :: Context) => Tag -> Integer
countTag C0 = 1
countTag C1 = 1
countTag X = 1
countTag Y = 1
countTag Z = 1
countTag (Not tag) = countTag tag
countTag (Shl1 tag) = countTag tag
countTag (Shr1 tag) = countTag tag
countTag (Shr4 tag) = countTag tag
countTag (Shr16 tag) = countTag tag
countTag (And tag1 tag2) = countTag tag1 * countTag tag2
countTag (Or tag1 tag2) = countTag tag1 * countTag tag2
countTag (Xor tag1 tag2) = countTag tag1 * countTag tag2
countTag (Plus tag1 tag2) = countTag tag1 * countTag tag2
countTag (If0 tag1 tag2 tag3) = countTag tag1 * countTag tag2 * countTag tag3
countTag (Fold tag1 tag2 tag3) = countTag tag1 * countTag tag2 * countTag tag3
countTag (UF n) = (inFoldExprCounts ?ctx) ! n
countTag (AFS n) = (topLevelNoFoldCounts ?ctx) ! n
countTag (AF n) = (topLevelCounts ?ctx) ! n
countTag (TF n) = (inTFoldExprCounts ?ctx) ! n

main = do
    putStrLn "size,inFoldExprCount,topLevelNoFoldCount,topLevelCount,tfoldCount"
    forM_ [1..size] $ \s ->
      printf "%d,%d,%d,%d,%d\n" s (countTag (UF s)) (countTag (AFS s)) (countTag (AF s)) (countTag (TF s))
  where
    ?ctx = defaultContext
