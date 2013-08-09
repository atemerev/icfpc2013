module Main where

import System.Environment
import StringClient (getMyproblems, getStatus, getAnyTrainingProblem, evalProgram, guessProgram)
import Options.Applicative

data Cmd = MyProblems
         | Status
         | Train
         | Eval String [String]
         | Guess String String

main = execParser options >>= run

run MyProblems = putStrLn =<< getMyproblems
run Status = putStrLn =<< getStatus
run Train = putStrLn =<< getAnyTrainingProblem -- TODO: add args
run (Eval program args) =  putStrLn =<< evalProgram program (map read args) -- TODO: add args
run (Guess id program) = putStrLn =<< guessProgram id program -- TODO: add args

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
    (info (pure Train)
     (progDesc "Get random task for training"))
  <> command "eval"
    (info eval
     (progDesc "Eval program on given inputs"))
  <> command "guess"
    (info guess
     (progDesc "Provide guess for given program ID"))
  )

eval = Eval <$> argument str (metavar "PROGRAM")
            <*> arguments str (metavar "ARGS ...")

guess = Guess <$> argument str (metavar "ID")
              <*> argument str (metavar "PROGRAM")

