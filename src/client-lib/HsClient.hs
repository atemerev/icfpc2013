module HsClient ( -- getMyproblems
                -- , getStatus
                 evalProgram
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

wrap stringClient = do
  response <- stringClient
  return $ fromMaybe (error "decode failed") $ decode $ BS.pack response
  
-- getMyproblems           = wrap $ SC.getMyproblems
-- getStatus               = wrap $ SC.getStatus
evalProgram pgm args    = wrap $ SC.evalProgram pgm args
evalProgramById id args = wrap $ SC.evalProgramById id args
guessProgram id pgm     = wrap $ SC.guessProgram id pgm
getTrainingProblem s o  = wrap $ SC.getTrainingProblem s o

getAnyTrainingProblem :: IO TrainingResponse
getAnyTrainingProblem   = wrap $ SC.getAnyTrainingProblem
