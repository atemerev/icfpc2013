import Types
import Gen
import HsClient
import ServerAPI
import RandomBV
import Filter
import PP
import Data.Maybe

targetSize = 12

getProblem :: IO TrainingResponse
getProblem = do
  getTrainingProblem (Just targetSize) Nothing

progs :: [String] -> [Exp]
progs ops = generateRestricted targetSize ops

main = do
  p <- getProblem
  let progId = (trainingId p)
  print p
  EvalOK outputs <- evalProgramById progId bvs
  print outputs
  loop progId bvs outputs (allProgs p)
  
  where 
    allProgs p = (progs (trainingOps p))
    
    loop pId inputs outputs programs = do
      let
        candidates = filterProgs inputs outputs programs
        first = head candidates
        -- mapM_ (putStrLn . ppProg) candidates
      print first
      gr <- guessProgram pId (ppProg first)
      case gr of
        Win -> print gr
        Mismatch input expected actual -> do
          putStrLn $ "Mismatch on " ++ show input ++ " : " ++ show actual ++ " instead of " ++ show expected
          putStrLn $ "Continuing with " ++ show (length candidates) ++ " candidates left"
          loop pId (input:inputs) (expected:outputs) candidates
        GuessError err -> error $ "guess error: " ++ err
