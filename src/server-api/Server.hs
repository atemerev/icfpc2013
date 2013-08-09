module Main where

import System.Environment
import ServerAPI (getMyproblems)

main = do
  args <- getArgs
  case args of
    ["myproblems"] -> putStrLn =<< getMyproblems
