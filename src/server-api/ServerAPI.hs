{-# LANGUAGE OverloadedStrings #-}
module ServerAPI ( getMyproblems
                 , getStatus
                 , getTrainingProblem
                 , getAnyTrainingProblem -- no size/ops limit
                 ) where

import Control.Applicative
import Data.Aeson
import qualified Data.Vector as V
import Network.HTTP hiding (postRequest)
import qualified URLs
import Data.Word
import Data.ByteString.Lazy.Char8 as BS hiding (map)

data Problem = Problem { problemId :: String
                       , problemSize :: Int
                       , operators :: [String]    -- TODO(vanya) This should be [Operator]
                       , solved :: Maybe Bool
                       , timeLeft :: Maybe Double
                       } deriving Show

data EvalRequest = EvalRequest { evalProgramId :: Maybe String
                               , evalProgram :: Maybe String
                               , evalArguments :: [Word64]
                               } deriving Show

data EvalResponse = EvalResponse { evalStatus :: String
                                 , evalOutputs :: Maybe [String]
                                 , evalMessage :: Maybe String
                                 } deriving Show

data Guess = Guess { guessProblemId :: String
                   , guessProgram :: String -- TODO(vanya) should be Program
                   } deriving Show

data GuessResponse = GuessResponse { guessStatus :: String
                                   , guessValues :: Maybe [Word64]
                                   , guessMessage :: Maybe String
                                   } deriving Show
instance FromJSON Problem where
  parseJSON (Object v) = Problem <$>
                         v .: "id" <*>
                         v .: "size" <*>
                         v .: "operators" <*>
                         v .:? "solved" <*>
                         v .:? "timeLeft"

instance FromJSON EvalRequest where
  parseJSON (Object v) = EvalRequest <$>
                         v .:? "id" <*>
                         v .:? "program" <*>
                         (map read <$> v .: "arguments")

instance FromJSON EvalResponse where
  parseJSON (Object v) = EvalResponse <$>
                         v .: "status" <*>
                         v .:? "program" <*>
                         v .:? "message"

instance FromJSON Guess where
  parseJSON (Object v) = Guess <$>
                         v .: "id" <*>
                         v .: "program"

instance FromJSON GuessResponse where
  parseJSON (Object v) = GuessResponse <$>
                         v .: "status" <*>
                         (fmap (map read) <$> (v .:? "values")) <*>
                         v .:? "message"

arglessRequest url = do
  rsp <- Network.HTTP.simpleHTTP (getRequest url)
         -- fetch document and return it (as a 'String'.)
  (2,0,0) <- getResponseCode rsp -- lamely ensure that we haven't got errors
  getResponseBody rsp

postRequest url body = do
  rsp <- Network.HTTP.simpleHTTP (postRequestWithBody url "text/json" (BS.unpack (encode body)))
         -- fetch document and return it (as a 'String'.)
  (2,0,0) <- getResponseCode rsp -- lamely ensure that we haven't got errors
  getResponseBody rsp
  
----------------------
-- 1. Getting problems
getMyproblems = arglessRequest URLs.myproblems
  
-- 2. Evaluating programs
-- 3. Submitting guesses

--------------
-- 4. Training
-- From IRC: 
-- <ArchVince> with fold anything under 11 fails for me
-- <ArchVince> with tfold anything under 8
data OpLimit = NoFolds | Fold | TFold deriving Show
data TrainingReq = TrainingReq { size :: Maybe Int
                               , opLimit :: Maybe OpLimit
                               } deriving Show

array lst = Array $ V.fromList lst

instance ToJSON OpLimit where
  toJSON NoFolds = array []
  toJSON Fold    = array ["fold"]
  toJSON TFold   = array ["tfold"]
  
instance ToJSON TrainingReq where
  toJSON (TrainingReq sz op) = object [ "size" .= sz, "operators" .= op ]
  
getTrainingProblem sz oplimit = 
  postRequest URLs.train (TrainingReq sz oplimit)

getAnyTrainingProblem = getTrainingProblem Nothing Nothing
------------
-- 5. Status
getStatus = arglessRequest URLs.status

