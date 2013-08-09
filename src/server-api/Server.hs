module Main where

import System.Environment
import ServerAPI (getMyproblems, getStatus, getAnyTrainingProblem)

main = do
  args <- getArgs
  case args of
    ["myproblems"] -> putStrLn =<< getMyproblems
    ["status"] -> putStrLn =<< getStatus
    ["train"] -> putStrLn =<< getAnyTrainingProblem -- TODO: add args
