{-# LANGUAGE ImplicitParams #-}
module ProgramCounting (
  main,
  countTag,
  expandTag,
  Context,
  newContext,
  newContextFromStrings,
  defaultContext, -- ^ all operations allowed
  defaultAllowedOp1,
  defaultAllowedOp2,
  EvalContext,
  evalCtx,
  eval,
  expectedComplexity,
  denumeralize,
  buildCaches,
  getCached,
  getInputIdx,
  getNumCachedProgs,
  allFunctionsSpace,
  findAllCachedMatches,
  tag2expr
) where
import System.Environment (getArgs)

import Data.Array
import qualified Data.Array.Unboxed as AU
import Data.Array.IArray (amap)
import Data.List (sort, nub, findIndex)
import Control.Monad
import Text.Printf
import Data.Bits
import Data.Word
import Data.Map (Map)
import qualified Data.Map as M
import qualified Types as T

size = 43
allFunctionsSpace = [ AF n | n <- [1..size] ]

--inFoldExprCount, topLevelNoFoldCount, topLevelCount, tfoldCount :: Array Int Integer

type D = Int -- depth
data Tag = UF D | AFS D | AF D | TF D
         | C0 | C1 | X | Y | Z
         | Not Tag | Shl1 Tag | Shr1 Tag | Shr4 Tag | Shr16 Tag
         | And Tag Tag | Or Tag Tag | Xor Tag Tag | Plus Tag Tag
         | If0 Tag Tag Tag | Fold Tag Tag Tag
         deriving (Show, Eq, Ord)


tag2expr :: Tag -> T.ExpC
tag2expr C0 = T.zero
tag2expr C1 = T.one
tag2expr X = T.mainArg
tag2expr Y = T.fold1Arg
tag2expr Z = T.fold2Arg
tag2expr (Not x) = T.not_ (tag2expr x)
tag2expr (Shl1 x) = T.shl1 (tag2expr x)
tag2expr (Shr4 x) = T.shr4 (tag2expr x)
tag2expr (Shr16 x) = T.shr16 (tag2expr x)
tag2expr (And a b) = T.and_ (tag2expr a) (tag2expr b)
tag2expr (Or a b) = T.and_ (tag2expr a) (tag2expr b)
tag2expr (Xor a b) = T.and_ (tag2expr a) (tag2expr b)
tag2expr (Plus a b) = T.and_ (tag2expr a) (tag2expr b)
tag2expr (If0 a b c) = T.if0 (tag2expr a) (tag2expr b) (tag2expr c)
tag2expr (Fold a b c) = T.fold_ (tag2expr a) (tag2expr b) (tag2expr c)


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

newContextFromStrings :: [String] -> Context
newContextFromStrings operations = newContext op1 op2 ifOk foldOk
  where
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
expectedComplexity operations size_ =
  if size_ > size then error "increase size in ProgramCounting.hs"
  else let ?ctx = newContextFromStrings operations in
          sum (map countTag tags)
  where
    isTFold = "tfold" `elem` operations
    tags | isTFold = [ TF i | i <- [1..size_-4] ]
         | otherwise = [ AF i | i <- [1..size_] ]

isBasicExpr :: Tag -> Bool
isBasicExpr UF{} = False
isBasicExpr AFS{} = False
isBasicExpr AF{} = False
isBasicExpr TF{} = False
isBasicExpr C0 = True
isBasicExpr C1 = True
isBasicExpr X = True
isBasicExpr Y = True
isBasicExpr Z = True
isBasicExpr (Not tag) = isBasicExpr tag
isBasicExpr (Shl1 tag) = isBasicExpr tag
isBasicExpr (Shr1 tag) = isBasicExpr tag
isBasicExpr (Shr4 tag) = isBasicExpr tag
isBasicExpr (Shr16 tag) = isBasicExpr tag
-- tag2 usually has smaller size
isBasicExpr (And tag1 tag2) = isBasicExpr tag2 && isBasicExpr tag1
isBasicExpr (Or tag1 tag2) = isBasicExpr tag2 && isBasicExpr tag1
isBasicExpr (Xor tag1 tag2) = isBasicExpr tag2 && isBasicExpr tag1
isBasicExpr (Plus tag1 tag2) = isBasicExpr tag2 && isBasicExpr tag1
isBasicExpr (If0 tag1 tag2 tag3) = isBasicExpr tag1 && isBasicExpr tag2 && isBasicExpr tag3
isBasicExpr (Fold tag1 tag2 tag3) = isBasicExpr tag1 && isBasicExpr tag2 && isBasicExpr tag3

isConst :: Tag -> Bool
isConst UF{} = False
isConst AFS{} = False
isConst AF{} = False
isConst TF{} = False
isConst C0 = True
isConst C1 = True
isConst (Not tag) = isConst tag
isConst (Shl1 tag) = isConst tag
isConst (Shr1 tag) = isConst tag
isConst (Shr4 tag) = isConst tag
isConst (Shr16 tag) = isConst tag
-- tag2 usually has smaller size
isConst (And tag1 tag2) = isConst tag2 && isConst tag1 || tag1 == C0 || tag2 == C0
isConst (Or tag1 tag2) = isConst tag2 && isConst tag1
isConst (Xor tag1 tag2) = isConst tag2 && isConst tag1
isConst (Plus tag1 tag2) = isConst tag2 && isConst tag1
isConst (If0 tag1 tag2 tag3) = isConst tag1 && isConst tag2 && isConst tag3
isConst (Fold tag1 tag2 tag3) = isConst tag1 && isConst tag2 && isConst tag3
isConst _ = False

denumeralize :: (?ctx :: Context) => Integer -> [Tag] -> Tag
denumeralize x ts | length ts == 1 && isBasicExpr firstElem = firstElem
                  | otherwise = denumeralize (x-base) (expandTag xType)
  where
    firstElem = head ts
    counts = map countTag ts
    accCounts = scanl (+) 0 counts
    precedingCounts = takeWhile (<=x) accCounts
    xType = ts !! (length precedingCounts - 1)
    base = last precedingCounts

data InterleavedCache = ICache !(AU.UArray Int Word64) !Int !Int [Word64] deriving Show
getInputIdx :: InterleavedCache -> Word64 -> Maybe Int
getInputIdx (ICache _ _ _ inputs) v = findIndex (==v) inputs

getNumCachedProgs :: InterleavedCache -> Int
getNumCachedProgs (ICache _ n _ _) = n

getCached :: InterleavedCache -> Int -> Int -> Word64
getCached (ICache vs cSize nInputs inputs) progId inputIdx = vs AU.! (nInputs*progId + inputIdx)

findAllCachedMatches :: InterleavedCache -> Int -> Word64 -> [Int]
findAllCachedMatches (ICache vs cSize nInputs inputs) outputIdx output =
  [ progId | progId <- [0..cSize-1], let v = vs AU.! (progId*nInputs + outputIdx), v == output ]

buildCaches :: (?ctx :: Context) => [Word64] -> Int -> InterleavedCache
buildCaches inputs cacheSize = ICache (AU.listArray (0,iCacheSize-1) computedValues) cacheSize nInputs inputs
  where
    nInputs = length inputs
    iCacheSize = (cacheSize*nInputs)
    computedValues = [ evalProg prog x
                     | n <- [0..cacheSize-1]
                     , let prog = denumeralize (fromIntegral n) allFunctionsSpace
                     , x <- inputs
                     ]
    evalProg program x = eval program 
      where
        ?ectx = evalCtx x 0 0
  
main = do
    [sizeStr] <- getArgs
    let ts = [AF n | n <- [1..42]]
        size = read sizeStr
        maxN = sum $ map countTag [AF n | n <- [1..size]]
        inputV = [0x1122334455667788, 0xFEDCBA9876543210, 1]
        ICache cache _ _ _ = buildCaches inputV (fromIntegral maxN)
    print maxN
    print $ map (\i -> cache AU.! (fromIntegral $ (i+1)*maxN - 1)) [0,1,2]
  where
    ?ctx = newContext defaultAllowedOp1 defaultAllowedOp2 True False

main' = do
    putStrLn "size,inFoldExprCount,topLevelNoFoldCount,topLevelCount,tfoldCount"
    forM_ [1..size] $ \s ->
      printf "%d,%d,%d,%d,%d\n" s (countTag (UF s)) (countTag (AFS s)) (countTag (AF s)) (countTag (TF s))
  where
    ?ctx = defaultContext
