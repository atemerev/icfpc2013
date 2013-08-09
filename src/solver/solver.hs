import Types
import Gen
import HsClient
import ServerAPI
import RandomBV
import Filter
import PP
import Data.Maybe

getProblem :: IO TrainingResponse
getProblem = do
  getTrainingProblem (Just 8) Nothing

progs :: [Exp]
progs = generateAll 8

main = do
  p <- getProblem
  let progId = (trainingId p)
  print p
  EvalOK outputs <- evalProgramById progId bvs
  print outputs

  let
    candidates = filterProgs bvs outputs progs
    first = head candidates -- XXX
  -- mapM_ (putStrLn . ppProg) candidates
  print first

  gr <- guessProgram progId (ppProg first)
  print gr
