module HttpClient ( getMyproblems
                 , getStatus
                 , evalProgram
                 , evalProgramById
                 , guessProgram
                 , getTrainingProblem
                 , getAnyTrainingProblem -- no size/ops limit
                 ) where

import qualified URLs
import ServerAPI
import Network.HTTP hiding (postRequest)
import Data.ByteString.Lazy.Char8 as BS hiding (map)
import Data.Aeson 

----------------------
-- 1. Getting problems
getMyproblems = arglessRequest URLs.myproblems
  
-------------------------
-- 2. Evaluating programs
evalProgramById id vals  = postRequest URLs.eval (EvalRequest (ID id) vals)
evalProgram pgm vals     = postRequest URLs.eval (EvalRequest (Program pgm) vals)

------------------------
-- 3. Submitting guesses
guessProgram id pgm = postRequest URLs.guess (Guess id pgm)

--------------
-- 4. Training
-- From IRC: 
-- <ArchVince> with fold anything under 11 fails for me
-- <ArchVince> with tfold anything under 8
getTrainingProblem sz oplimit = 
  postRequest URLs.train (TrainingRequest sz oplimit)

getAnyTrainingProblem = getTrainingProblem Nothing Nothing
------------
-- 5. Status
getStatus = arglessRequest URLs.status


---
-- Actual HTTP Requests
responseToString rsp = do
  rspCode <- getResponseCode rsp
  case rspCode of
    -- lamely ensure that we haven't got errors
    (2,0,0) -> getResponseBody rsp
    (x,y,z) -> error $ "HTTP response " ++ show (x*100+y*10+z)
  
arglessRequest url = do
  rsp <- Network.HTTP.simpleHTTP (getRequest url)
  responseToString rsp
  
postRequest url body = do
  rsp <- Network.HTTP.simpleHTTP (postRequestWithBody url "text/json" (BS.unpack (encode body)))
  responseToString rsp
  
