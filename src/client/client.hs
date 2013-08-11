module Main where

import System.Environment
import Options.Applicative
import ServerAPI (OpLimit(..),Problem(..), TrainingResponse(..))
import StringClient as SC (getMyproblems, getStatus, getTrainingProblem, evalProgram, evalProgramById, guessProgram)
import FileClient as FC (getUnsolved, getUnsolvedHS, filterByIds, getMyproblemsHS)
import HsClient as HC (getTrainingProblem, getUnsolved)
import ProgramCounting as PC (expectedComplexity)
import Gen
import Data.List (isInfixOf, intercalate, find, sortBy)
import Data.Ord (comparing)
import Text.Printf
import System.IO
import Solve (solve, isFeasible, solveExact, solveWithTimeout)
import PP (ppProg)
import Data.Word
import Filter
import Data.Maybe
import Data.Time.Clock

data Cmd = MyProblems
         | Status
         | Train (Maybe Int) (Maybe OpLimit)
         | Eval String [String]
         | Guess String String
         | Generate Int [String]
         | Unsolved String
         | FindSolvable String Int
         | TrainSolve Int
         | Solve Int String String
         | Filter String String
         | SolveMany Int Int Int Bool Bool
         | LowLevelSolve String Int [String]
         | SolveExact Int [String] ([Word64], [Word64])
         | FilterCached Int [String] Word64
         | EstimateComplexity Int [String]
         | ReportComplexity String Bool
           
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

run (Generate size ops) = mapM_ (putStrLn . ppProg) $ generateRestrictedUpTo size ops (64, 64) False Nothing

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
        Just () -> pp (p, generateRestricted (problemSize p) (operators p) (64, 64) False Nothing)
      tryGen ps
    pp (p, exps) = putStrLn $ (printf "%s|%d|%s|%d" (problemId p) (problemSize p) (intercalate " " $ operators p) (length exps))

run (TrainSolve size) = do
  p <- HC.getTrainingProblem (Just size) Nothing
  let progId = (trainingId p)
  print p
  solve (trainingId p) size (trainingOps p)

run (Solve tmout fname id) = do
  problems <- FC.getUnsolvedHS fname
  case find ((==id).problemId) problems of
    Nothing -> error "No unsolved problems with this ID"
    Just p -> solveWithTimeout tmout (problemId p) (problemSize p) (operators p)

run (LowLevelSolve id size ops) = solve id size ops

run (SolveExact size ops (ins, outs)) = solveExact size ops ins outs

run (Filter problemsFile idsFile) = FC.filterByIds problemsFile idsFile >>= putStrLn
  
run (SolveMany offset limit tmout bySize includeBonuses) = do
  problemsUnfiltered <- HC.getUnsolved
  let problems = if includeBonuses then problemsUnfiltered else filter (not.("bonus" `elem`).operators) problemsUnfiltered
  let workload = take limit $ drop offset $ sortProblems problems
  trySolve workload
  where
    sortProblems = if bySize then sortBy (comparing problemSize)
                     else sortBy (comparing complexity)
    complexity p = PC.expectedComplexity (operators p) (problemSize p)
    
    trySolve [] =  putStrLn "All done!"
    trySolve (p:ps) = do
      tm <- getCurrentTime
      print tm
      putStrLn $ printf ">>> Trying %s, size %d, operations (%s)" (problemId p) (problemSize p) (intercalate " " $ operators p)
      solveWithTimeout tmout (problemId p) (problemSize p) (operators p)
      trySolve ps

run (FilterCached size ops expected) = mapM_ (putStrLn.ppProg) $ filterByCached expected $ generateRestrictedUpTo size ops (64, 64) False (Just expected)

run (EstimateComplexity size operations) = putStrLn $ show $ PC.expectedComplexity operations size

run (ReportComplexity fname all) = do
  problems <- if all then FC.getMyproblemsHS fname else FC.getUnsolvedHS fname
  mapM_ report $ sortBy (comparing fst) $ zip (map complexity problems) problems
  where
    complexity p = PC.expectedComplexity (operators p) (problemSize p)
    report :: (Integer,Problem) -> IO ()
    report (c,p) = putStrLn $ printf "%s|%s|%d|%s|%0.5f" (maybe "not" (\b -> if b then "yes" else "not")  $ solved p) 
                   (problemId p) (problemSize p) (intercalate " " $ operators p) ((log $ fromIntegral c) :: Double)

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
  <> command "solve-exact"
    (info solveExactCmd
     (progDesc "Try solving problem supplying its SIZE, OPERATIONS, INPUTS/OUTPUTS manually"))
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
  <> command "estimate-complexity"
    (info estimateComplexity
     (progDesc "Estimate complexity of a problem, based on its SIZE and OPERATIONS"))
  <> command "report-complexity"
    (info reportComplexity
     (progDesc "Report all problems in the order of their complexity"))
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

solveExactCmd = SolveExact <$> (read <$> argument str (metavar "SIZE"))
                           <*> (words <$> (argument str (metavar "OPERATIONS")))
                           <*> (read <$> (argument str (metavar "INOUTS")))

filterCached = FilterCached <$> (read <$> argument str (metavar "SIZE"))
                            <*> (words <$> (argument str (metavar "OPERATIONS")))
                            <*> (read <$> argument str (metavar "EXPECTED-VALUE"))

realSolve = Solve <$> (read <$> argument str (metavar "TIMEOUT"))
                  <*> argument str (metavar "FILE")
                  <*> argument str (metavar "ID")

realSolveMany= SolveMany <$> (read <$> strOption (metavar "OFFSET" <> long "offset"))
                         <*> (read <$> strOption (metavar "LIMIT" <> long "limit"))
                         <*> (read <$> strOption (metavar "TIMEOUT" <> long "timeout"))
                         <*> (switch (long "by-size" <> help "order tasks by size, not by complexity (faster)" <> value False))
                         <*> (switch (long "include-bonuses" <> help "include bonus tasks" <> value False))

filterProblems = Filter <$> argument str (metavar "MYPROBLEMS-FILE")
                        <*> argument str (metavar "IDS-FILE")

estimateComplexity = EstimateComplexity <$> (read <$> argument str (metavar "SIZE"))
                                        <*> (words <$> (argument str (metavar "OPERATIONS")))

reportComplexity = ReportComplexity <$> (argument str (metavar "FILE"))
                                    <*> (switch (long "all" <> help "all tasks, not just unsolved" <> value False))
