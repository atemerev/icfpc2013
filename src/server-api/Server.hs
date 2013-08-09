module Main where

import System.Environment
import ServerAPI (getMyproblems, getStatus, getAnyTrainingProblem, evalProgram)

main = do
  args <- getArgs
  case args of
    ["myproblems"] -> putStrLn =<< getMyproblems
    ["status"] -> putStrLn =<< getStatus
    ["train"] -> putStrLn =<< getAnyTrainingProblem -- TODO: add args
    ("eval":program:rest) -> putStrLn =<< evalProgram program (map read rest) -- TODO: add args
