{-# LANGUAGE BangPatterns #-}
import Test.SmallCheck.Series
import Types
import Gen

stat ps = stat' 0 0 ps
stat' !n !s [] = (n, s)
stat' !n !s (p:ps) = stat' (n+1) (s + 1 {-eval 0 0 0 p-} ) ps

main = print $ length $ list 10 (serProg noRestriction)
