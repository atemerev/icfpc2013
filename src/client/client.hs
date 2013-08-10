module Main where

import System.Environment
import Options.Applicative
import ServerAPI (OpLimit(..),Problem(..), TrainingResponse(..))
import StringClient as SC (getMyproblems, getStatus, getTrainingProblem, evalProgram, evalProgramById, guessProgram)
import FileClient as FC (getUnsolved, getUnsolvedHS, filterByIds)
import HsClient as HC (getTrainingProblem, getUnsolved)
import Gen
import Data.List (isInfixOf, intercalate, find, sortBy)
import Data.Ord (comparing)
import Text.Printf
import System.IO
import Solve (solve, isFeasible, solve')
import PP (ppProg)
import Data.Word
import Filter

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
         | Filter String String
         | SolveMany Int Int Int
         | LowLevelSolve String Int [String]
         | FilterCached Int [String] Word64
           
           
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

run (Generate size ops) = mapM_ (putStrLn . ppProg) $ generateRestrictedUpTo size ops

run (Unsolved fname) = putStrLn =<< FC.getUnsolved fname

run (FindSolvable fname tmout) = do
  problems <- FC.getUnsolvedHS fname
  tryGen $ sortBy (comparing problemSize) problems
  where
    tryGen [] =  return ()
    tryGen (p:ps) = do
      res <- isFeasible tmout p
      _ <- case res of  
        Nothing -> return () -- putStrLn ("skipping " ++ problemId p ++ " - timed out")
        Just () -> pp (p, generateRestricted (problemSize p) (operators p))
      tryGen ps
    pp (p, exps) = putStrLn $ (printf "%s|%d|%s|%d" (problemId p) (problemSize p) (intercalate " " $ operators p) (length exps))

run (TrainSolve size) = do
  p <- HC.getTrainingProblem (Just size) Nothing
  let progId = (trainingId p)
  print p
  solve (trainingId p) size (trainingOps p)

run (Solve fname id) = do
  problems <- FC.getUnsolvedHS fname
  case find ((==id).problemId) problems of
    Nothing -> error "No unsolved problems with this ID"
    Just p -> solve (problemId p) (problemSize p) (operators p)

run (LowLevelSolve id size ops) = solve id size ops

run (Filter problemsFile idsFile) = FC.filterByIds problemsFile idsFile >>= putStrLn
  
run (SolveMany offset limit tmout) = do
  problems <- HC.getUnsolved
  let workload = take limit $ drop offset $ sortBy (comparing problemSize) problems
  trySolve workload
  where
    trySolve [] =  putStrLn "All done!"
    trySolve (p:ps) = do
      putStrLn $ printf "Trying task %s, size %d, operations (%s)" (problemId p) (problemSize p) (intercalate " " $ operators p)
      res <- isFeasible tmout p
      _ <- case res of  
        Nothing -> putStrLn ("  skipping " ++ problemId p ++ " - timed out")
        Just () -> solve' (problemId p) (generateRestricted (problemSize p) (operators p))
      trySolve ps

run (FilterCached size ops expected) = mapM_ (putStrLn.ppProg) $ filterByCached expected $ generateRestrictedUpTo size ops


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
     (progDesc "Find list of problems solvable by brute-force within N seconds (deprecated)"))
  <> command "train-solve"
    (info trainSolve
     (progDesc "Solve the new training task of the given size"))
  <> command "low-level-solve"
    (info lowLevelSolve
     (progDesc "Try solving problem supplying its ID, SIZE, OPERATIONS manually"))
  <> command "production-solve-one"
    (info realSolve
     (progDesc "Solve the REAL tasks with given ID (deprecated)"))
  <> command "production-solve-many"
    (info realSolveMany
     (progDesc "Solve some unsolved REAL tasks"))
  <> command "filter-out"
    (info filterProblems
     (progDesc "Filter out tasks with given IDs (deprecated)"))
  <> command "filter-cached"
    (info filterCached
     (progDesc "Filter generated expressions by the value they should have on the first test input"))
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

lowLevelSolve = LowLevelSolve <$> argument str (metavar "ID")
                              <*> (read <$> argument str (metavar "SIZE"))
                              <*> (words <$> (argument str (metavar "OPERATIONS")))

filterCached = FilterCached <$> (read <$> argument str (metavar "SIZE"))
                            <*> (words <$> (argument str (metavar "OPERATIONS")))
                            <*> (read <$> argument str (metavar "EXPECTED-VALUE"))

realSolve = Solve <$> argument str (metavar "FILE")
                  <*> argument str (metavar "ID")

realSolveMany= SolveMany <$> (read <$> strOption (metavar "OFFSET" <> long "offset"))
                         <*> (read <$> strOption (metavar "LIMIT" <> long "limit"))
                         <*> (read <$> strOption (metavar "TIMEOUT" <> long "timeout"))

filterProblems = Filter <$> argument str (metavar "MYPROBLEMS-FILE")
                        <*> argument str (metavar "IDS-FILE")
