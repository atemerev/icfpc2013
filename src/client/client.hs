module Main where

import System.Environment
import Options.Applicative
import ServerAPI (OpLimit(..),Problem(..))
import StringClient as SC (getMyproblems, getStatus, getTrainingProblem, evalProgram, evalProgramById, guessProgram)
import FileClient as FC (getUnsolved)
import Gen
import Data.List (isInfixOf)

data Cmd = MyProblems
         | Status
         | Train { length :: Maybe Int, opLimit::Maybe OpLimit }
         | Eval String [String]
         | Guess String String
         | Generate Int [String]
         | Unsolved String

main = execParser options >>= run

run MyProblems = putStrLn =<< SC.getMyproblems
run Status = putStrLn =<< getStatus
run (Train length oplimit) = putStrLn =<< getTrainingProblem length oplimit
run (Eval programOrId args) = 
  if " " `isInfixOf` programOrId
  then putStrLn =<< evalProgram     programOrId (map read args)
  else putStrLn =<< evalProgramById programOrId (map read args)
run (Guess id program) = putStrLn =<< guessProgram id program
run (Generate size ops) = mapM_ print $ generateRestricted size ops
run (Unsolved fname) = putStrLn =<< FC.getUnsolved fname

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
