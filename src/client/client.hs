module Main where

import System.Environment
import Options.Applicative
import ServerAPI (OpLimit(..),Problem(..), TrainingResponse(..))
import StringClient as SC (getMyproblems, getStatus, getTrainingProblem, evalProgram, evalProgramById, guessProgram)
import FileClient as FC (getUnsolved, getUnsolvedHS)
import HsClient as HC (getTrainingProblem)
import Gen
import Data.List (isInfixOf, intercalate)
import Text.Printf
import System.IO
import Solve (solve, isFeasible)

data Cmd = MyProblems
         | Status
         | Train (Maybe Int) (Maybe OpLimit)
         | Eval String [String]
         | Guess String String
         | Generate Int [String]
         | Unsolved String
         | FindSolvable String Int
         | TrainSolve Int
         | Solve String String
           
main = do
  hSetBuffering stdout NoBuffering
  execParser options >>= run

run MyProblems = putStrLn =<< SC.getMyproblems
run Status = putStrLn =<< getStatus
run (Train length oplimit) = putStrLn =<< SC.getTrainingProblem length oplimit
run (Eval programOrId args) = 
  if " " `isInfixOf` programOrId
  then putStrLn =<< evalProgram     programOrId (map read args)
  else putStrLn =<< evalProgramById programOrId (map read args)
run (Guess id program) = putStrLn =<< guessProgram id program
run (Generate size ops) = mapM_ print $ generateRestricted size ops
run (Unsolved fname) = putStrLn =<< FC.getUnsolved fname
run (FindSolvable fname tmout) = do
  problems <- FC.getUnsolvedHS fname
  tryGen problems
  where
    tryGen [] =  return ()
    tryGen (p:ps) = do
      res <- isFeasible tmout p
      _ <- case res of  
        Nothing -> return () -- putStrLn ("skipping " ++ problemId p ++ " - timed out")
        Just rs -> pp rs
      tryGen ps
    pp (p, sz) = putStrLn $ (printf "%s|%d|%s|%d" (problemId p) (problemSize p) (intercalate " " $ operators p) sz)
run (TrainSolve size) = do
  p <- HC.getTrainingProblem (Just size) Nothing
  let progId = (trainingId p)
  print p
  solve (trainingId p) size (trainingOps p)


options = info (clientOptions <**> helper) idm

clientOptions = 
  subparser
  ( command "myproblems"
    (info (pure MyProblems)
     (progDesc "Get our problem set"))
  <> command "status"
    (info (pure Status)
     (progDesc "Get current team status"))
  <> command "train"
    (info train
     (progDesc "Get random task for training"))
  <> command "eval"
    (info eval
     (progDesc "Eval program on given inputs"))
  <> command "guess"
    (info guess
     (progDesc "Provide guess for given program ID"))
  <> command "generate"
    (info generate
     (progDesc "Generate programs of given size with given ops restrictions"))
  <> command "unsolved"
    (info unsolved
     (progDesc "Provide the list of tasks that are still not solved"))
  <> command "find-solvable"
    (info findSolvable
     (progDesc "Find list of problems solvable by brute-force within N seconds"))
  <> command "train-solve"
    (info trainSolve
     (progDesc "Solve the new training task of the given size"))
  <> command "solve-production"
    (info realSolve
     (progDesc "Solve the REAL tasks with given ID"))
  )

train = Train <$> (fmap read <$> ( optional $ strOption (metavar "LENGTH" <> short 'l' <> long "length")))
              <*> (fmap read <$> ( optional $ strOption (metavar "NoFold|Fold|TFold" <> short 'o' <> long "ops")))
        
eval = Eval <$> argument str (metavar "PROGRAM or ID")
            <*> arguments1 str (metavar "ARG1 [ARG2 .. ARG_n]")

guess = Guess <$> argument str (metavar "ID")
              <*> argument str (metavar "PROGRAM")

generate = Generate <$> (read <$> argument str (metavar "SIZE"))
                    <*> (words <$> argument str (metavar "OPERATIONS"))

unsolved = Unsolved <$> argument str (metavar "FILE")

findSolvable = FindSolvable <$> argument str (metavar "FILE")
                            <*> (read <$> argument str (metavar "TIMEOUT"))

trainSolve = TrainSolve <$> (read <$> argument str (metavar "SIZE"))

realSolve = Solve <$> argument str (metavar "FILE")
                  <*> argument str (metavar "ID")
