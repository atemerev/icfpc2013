module Main where

import System.Environment
import ServerAPI (getMyproblems, getStatus)

main = do
  args <- getArgs
  case args of
    ["myproblems"] -> putStrLn =<< getMyproblems
    ["status"] -> putStrLn =<< getStatus
