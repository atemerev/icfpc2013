{-# LANGUAGE ImplicitParams #-}
module ProgramCounting (
  main,
  countTag,
  expandTag,
  Context,
  newContext,
  defaultContext, -- ^ all operations allowed
  defaultAllowedOp1,
  defaultAllowedOp2,
  EvalContext,
  evalCtx,
  eval,
  expectedComplexity
) where
import System.Environment (getArgs)

import Data.Array
import Data.Array.IArray (amap)
import Data.List (sort, nub)
import Control.Monad
import Text.Printf
import Data.Bits
import Data.Word

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

-- Use newContext to construct this!
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
                 , inFoldExprExpands = listArray (1,size) [ expandTag (UF n) | n <- [1..size] ]
                 , topLevelNoFoldExpands = listArray (1,size) [ expandTag (AFS n) | n <- [1..size] ]
                 , topLevelExpands = listArray (1,size) [ expandTag (AF n) | n <- [1..size] ]
                 , inTFoldExprExpands = listArray (1,size) [ expandTag (TF n) | n <- [1..size] ]
                 , inFoldExprCounts = amap (\expands -> sum $ map countTag expands) (inFoldExprExpands ctx)
                 , topLevelNoFoldCounts = amap (\expands -> sum $ map countTag expands) (topLevelNoFoldExpands ctx)
                 , topLevelCounts = amap (\expands -> sum $ map countTag expands) (topLevelExpands ctx)
                 , inTFoldExprCounts = amap (\expands -> sum $ map countTag expands) (inTFoldExprExpands ctx)
                 }) in ctx

expandTag :: (?ctx :: Context) => Tag -> [Tag]
expandTag (UF 1) = [C0, C1, X, Y, Z]
expandTag (UF n) = concat
  [ [ op1 (UF $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (UF i) (UF j) | op2 <- allowedOp2 ?ctx, i <- [1..n-2], let j = n-1-i, j <= i ]
  , [ If0 (UF i) (UF j) (UF $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]

expandTag (AFS 1) = [C0, C1, X]
expandTag (AFS n) = concat
  [ [ op1 (AFS $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (AFS i) (AFS j) | op2 <- allowedOp2 ?ctx, i <- [1..n-2], let j=n-1-i, j <= i ]
  , [ If0 (AFS i) (AFS j) (AFS $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]

expandTag af@(AF n) | isFoldAllowed ?ctx = expandTag' af
                    | otherwise = expandTag (AFS n)
  where
    expandTag' :: (?ctx :: Context) => Tag -> [Tag]
    expandTag' (AF 1) = []
    expandTag' (AF 2) = []
    expandTag' (AF 3) = []
    expandTag' (AF 4) = []
    expandTag' (AF n) = concat
      [ [ op1 (AF $ n-1) | op1 <- allowedOp1 ?ctx ]
      , [ op2 (AF i) (AFS j) | op2 <- allowedOp2 ?ctx, i <- [1..n-2], let j=n-1-i, i >= 5 ]
      , [ If0 (AF i) (AFS j) (AFS k) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i], let k=n-1-i-j, i >= 5 ]
      , [ If0 (AFS i) (AF j) (AFS k) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i], let k=n-1-i-j, j >= 5 ]
      , [ If0 (AFS i) (AFS j) (AF k) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i], let k=n-1-i-j, k >= 5 ]
      , [ Fold (AFS i) (AFS j) (UF k) | i <- [1..n-4], j <- [1..n-3-i], let k=n-2-i-j ]
      ]

expandTag (TF 1) = [C0, C1, Y, Z]   -- can't use X!
expandTag (TF n) = concat
  [ [ op1 (TF $ n-1) | op1 <- allowedOp1 ?ctx ]
  , [ op2 (TF i) (TF j) | op2 <- allowedOp2 ?ctx, i <- [1..n-2], let j=n-1-i, j <= i ]
  , [ If0 (TF i) (TF j) (TF $ n-1-i-j) | isIfAllowed ?ctx, i <- [1..n-3], j <- [1..n-2-i] ]
  ]

expandTag C0 = [C0]
expandTag C1 = [C1]
expandTag X = [X]
expandTag Y = [Y]
expandTag Z = [Z]
expandTag (Not tag) = map Not (expandTag tag)
expandTag (Shl1 tag) = map Shl1 (expandTag tag)
expandTag (Shr1 tag) = map Shr1 (expandTag tag)
expandTag (Shr4 tag) = map Shr4 (expandTag tag)
expandTag (Shr16 tag) = map Shr16 (expandTag tag)
expandTag (And tag1 tag2) = [ And a b | a <- expandTag tag1, b <- expandTag tag2 ]
expandTag (Or tag1 tag2) = [ Or a b | a <- expandTag tag1, b <- expandTag tag2 ]
expandTag (Xor tag1 tag2) = [ Xor a b | a <- expandTag tag1, b <- expandTag tag2 ]
expandTag (Plus tag1 tag2) = [ Plus a b | a <- expandTag tag1, b <- expandTag tag2 ]
expandTag (If0 tag1 tag2 tag3) = [ If0 a b c | a <- expandTag tag1, b <- expandTag tag2, c <- expandTag tag3 ]
expandTag (Fold tag1 tag2 tag3) = [ Fold a b c | a <- expandTag tag1, b <- expandTag tag2, c <- expandTag tag3 ]

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

data EvalContext = ECtx { x :: !Word64, y :: !Word64, z :: !Word64 }

eval :: (?ectx :: EvalContext) => Tag -> Word64
eval C0 = 0
eval C1 = 1
eval X = x ?ectx
eval Y = y ?ectx
eval Z = z ?ectx
eval (Not tag) = complement (eval tag)
eval (Shl1 tag) = shiftL (eval tag) 1
eval (Shr1 tag) = shiftR (eval tag) 1
eval (Shr4 tag) = shiftR (eval tag) 4
eval (Shr16 tag) = shiftR (eval tag) 16
eval (And tag1 tag2) = (eval tag1) .&. (eval tag2)
eval (Or tag1 tag2) = (eval tag1) .|. (eval tag2)
eval (Xor tag1 tag2) = (eval tag1) `xor` (eval tag2)
eval (Plus tag1 tag2) = (eval tag1) + (eval tag2)
eval (If0 tag1 tag2 tag3) = if eval tag1 == 0 then eval tag2 else eval tag3
eval (Fold tag1 tag2 tag3) = foldr foldOp seed [b1, b2, b3, b4, b5, b6, b7, b8]
  where
    foldOp :: (?ectx :: EvalContext) => Word64 -> Word64 -> Word64
    foldOp y z = let ?ectx = ?ectx { y = y, z = z } in eval (tag3)
    val = eval tag1
    seed = eval tag2
    b8 = val .&. 0xff
    b7 = (val `shiftR` 8) .&. 0xff
    b6 = (val `shiftR` 16) .&. 0xff
    b5 = (val `shiftR` 24) .&. 0xff
    b4 = (val `shiftR` 32) .&. 0xff
    b3 = (val `shiftR` 40) .&. 0xff
    b2 = (val `shiftR` 48) .&. 0xff
    b1 = (val `shiftR` 56) .&. 0xff

evalCtx :: Word64 -> Word64 -> Word64 -> EvalContext
evalCtx x y z = ECtx { x = x, y = y, z = z }

-- | Give the expected complexity score (how many possible functions exist, given the restrictions).
-- Example:
-- > expectedComplexity (words "not shl1 shr1 shr4 shr16 and or xor plus if0 fold tfold") 16
expectedComplexity :: [String] -> Int -> Integer
expectedComplexity operations size = let ?ctx = newContext op1 op2 ifOk foldOk in
                                       sum (map countTag tags)
  where
    tags | isTFold = [ TF i | i <- [1..size-4] ]
         | otherwise = [ AF i | i <- [1..size] ]
    addOp :: String -> a -> [a] -> [a]
    addOp op v vs | op `elem` operations = v:vs
                  | otherwise = vs
    op1 = addOp "not" Not $
          addOp "shl1" Shl1 $
          addOp "shr1" Shr1 $
          addOp "shr4" Shr4 $
          addOp "shr16" Shr16 $
          []
    op2 = addOp "and" And $
          addOp "or" Or $
          addOp "xor" Xor $
          addOp "plus" Plus $
          []
    ifOk = "if0" `elem` operations
    foldOk = "fold" `elem` operations
    isTFold = "tfold" `elem` operations

main = do
    n <- fmap (read.head) getArgs
    let res = (iterate (>>= expandTag) [AF n]) !! (n+1)
    print $ sum $ map (\f -> let ?ectx = evalCtx 0 0 0 in eval f) res
  where
    ?ctx = defaultContext

main' = do
    putStrLn "size,inFoldExprCount,topLevelNoFoldCount,topLevelCount,tfoldCount"
    forM_ [1..size] $ \s ->
      printf "%d,%d,%d,%d,%d\n" s (countTag (UF s)) (countTag (AFS s)) (countTag (AF s)) (countTag (TF s))
  where
    ?ctx = defaultContext
