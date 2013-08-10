{-# LANGUAGE BangPatterns #-}
import Test.SmallCheck.Series
import Types
import Gen

stat ps = stat' 0 0 ps
stat' !n !s [] = (n, s)
stat' !n !s (p:ps) = stat' (n+1) (s + eval 0 0 0 p) ps

main = print $ stat $ list 11 (serProg noRestriction)
