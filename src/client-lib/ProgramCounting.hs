module ProgramCounting (
  inFoldArgsCount,
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

inFoldArgsCount, inFoldExprCount, topLevelNoFoldCount, topLevelCount, tfoldCount :: Array Int Integer

inFoldArgsCount = ufnfC
inFoldExprCount = ufC
topLevelNoFoldCount = afsC
topLevelCount = afC
-- tfold is `(lambda (x) (fold x 0 (lambda (x y) e))`, and the only changing part is `e`, so tfold's size is the size of `e` + 5
tfoldCount = tfC


-------------------------------------------------------------------------------

ufnf, uf, afs, af, tf :: Int -> Integer
ufnfC, ufC, afsC, afC, tfC :: Array Int Integer

ufnfC = array (1,size) [(i, ufnf i) | i <- [1..size]]
afsC = array (1,size) [(i, afs i) | i <- [1..size]]
ufC = array (1,size) [(i, uf i) | i <- [1..size]]
tfC = array (1,size) [(i, tf i) | i <- [1..size]]
afC = array (1,size) [(i, af i) | i <- [1..size]]


-- expression in one of the first two arguments of the fold, can't use fold or y or z
ufnf 1 = 3                                                                                            -- 0 1 x
ufnf n = 5 * ufnfC ! (n-1) +                                                                          -- op1 ufnf
         4 * sum [ ufnfC ! i * ufnfC ! j | i <- [1..n-2], let j = n-1-i ] +                           -- op2 ufnf ufnf
         sum [ ufnfC ! i * ufnfC ! j * ufnfC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ]  -- if0 ufnf ufnf ufnf

-- expression under fold, can use everything except fold
uf 1 = 5                                                                                              -- 0 1 x y z
uf n = 5 * ufC ! (n-1) +                                                                              -- op1 uf
       4 * sum [ ufC ! i * ufC ! j | i <- [1..n-2], let j = n-1-i ] +                                 -- op2 uf uf
       sum [ ufC ! i * ufC ! j * ufC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ]          -- if0 uf uf uf

-- expression above fold, can't contain folds
afs 1 = 3                                                                                             -- 0 1 x
afs n = 5 * afsC ! (n-1) +                                                                            -- op1 afs
        4 * sum [ afsC ! i * afsC ! j | i <- [1..n-2], let j = n-1-i ] +                              -- op2 afs afs
        sum [ afsC ! i * afsC ! j * afsC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ]      -- if0 afs afs afs

-- expression above fold, can contain 1 fold
af 1 = 3                                                                                              -- 0 1 x
af n = 5 * afC ! (n-1) +                                                                              -- op1 af
       4 * 2 * sum [ afC ! i * afsC ! j | i <- [1..n-2], let j = n-1-i ] +                            -- op2 af afs, op2 afs af
       3 * sum [ afC ! i * afsC ! j * afsC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ] +  -- if0 af afs afs, if0 afs af afs, if0 afs afs af
       sum [ ufnfC ! i * ufnfC ! j * ufC ! k | i <- [1..n-3], j <- [1..n-2-i], let k = n-1-i-j ]      -- fold ufnf ufnf uf

-- expression in tfold, can't contain x, can't contain fold, size is minimum 6
tf n | n <= 5 = 0
     | n == 6 = 4                                                                                     -- 0 1 y z
     | otherwise = let m = n-5 in
       5 * ufC ! (m-1) +                                                                              -- op1 tf
       4 * sum [ ufC ! i * ufC ! j | i <- [1..m-2], let j = m-1-i ] +                                 -- op2 tf tf
       sum [ ufC ! i * ufC ! j * ufC ! k | i <- [1..m-3], j <- [1..m-2-i], let k = m-1-i-j ]          -- if0 tf tf tf

main = do
  putStrLn "size,inFoldArgsCount,inFoldExprCount,topLevelNoFoldCount,topLevelCount,tfoldCount"
  forM_ [1..size] $ \s ->
    printf "%d,%d,%d,%d,%d,%d\n" s (ufnfC ! s) (ufC ! s) (afsC ! s) (afC ! s) (tfoldCount !s)
