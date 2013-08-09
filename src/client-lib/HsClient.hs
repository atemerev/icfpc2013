module HsClient ( getMyproblems
                -- , getStatus
                , evalProgram
                , evalProgramById
                , guessProgram
                , getTrainingProblem
                , getAnyTrainingProblem -- no size/ops limit
                ) where

import qualified StringClient as SC
import Data.ByteString.Lazy.Char8 as BS hiding (map)
import Data.Aeson 
import Data.Maybe (fromMaybe)
import ServerAPI
import Data.Word (Word64)

wrap fname stringClient = do
  response <- stringClient
  return $ fromMaybe (error $ fname ++ ": decode failed on "++response) $ decode $ BS.pack response
  
getMyproblems :: IO [Problem]
getMyproblems = wrap "myproblems" $ SC.getMyproblems

-- getStatus               = wrap $ SC.getStatus

evalProgram     :: String -> [Word64] -> IO EvalResponse
evalProgramById :: String -> [Word64] -> IO EvalResponse
guessProgram    :: String -> String   -> IO GuessResponse
evalProgram pgm args    = wrap "evalProgram"        $ SC.evalProgram pgm args
evalProgramById id args = wrap "evalProgramById"    $ SC.evalProgramById id args
guessProgram id pgm     = wrap "guessProgram"       $ SC.guessProgram id pgm

getTrainingProblem :: Maybe Int -> Maybe OpLimit -> IO TrainingResponse
getTrainingProblem s o  = wrap "getTrainingProblem" $ SC.getTrainingProblem s o

getAnyTrainingProblem :: IO TrainingResponse
getAnyTrainingProblem = wrap "getAnyTrainingProblem" $ SC.getAnyTrainingProblem
