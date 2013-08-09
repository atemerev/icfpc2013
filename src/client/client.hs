module Main where

import System.Environment
import StringClient (getMyproblems, getStatus, getTrainingProblem, evalProgram, guessProgram)
import Options.Applicative
import ServerAPI (OpLimit(..))

data Cmd = MyProblems
         | Status
         | Train { length :: Maybe Int, opLimit::Maybe OpLimit }
         | Eval String [String]
         | Guess String String

main = execParser options >>= run

run MyProblems = putStrLn =<< getMyproblems
run Status = putStrLn =<< getStatus
run (Train length oplimit) = putStrLn =<< getTrainingProblem length oplimit -- TODO: add args
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
    (info train
     (progDesc "Get random task for training"))
  <> command "eval"
    (info eval
     (progDesc "Eval program on given inputs"))
  <> command "guess"
    (info guess
     (progDesc "Provide guess for given program ID"))
  )

train = Train <$> (fmap read <$> ( optional $ strOption (metavar "LENGTH" <> short 'l' <> long "length")))
              <*> (fmap read <$> ( optional $ strOption (metavar "NoFold|Fold|TFold" <> short 'o' <> long "ops")))
        
eval = Eval <$> argument str (metavar "PROGRAM")
            <*> arguments1 str (metavar "ARG1 [ARG2 .. ARG_n]")

guess = Guess <$> argument str (metavar "ID")
              <*> argument str (metavar "PROGRAM")

