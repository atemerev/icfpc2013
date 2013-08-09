{-# LANGUAGE OverloadedStrings #-}
module ServerAPI ( getMyproblems
                 , getStatus
                 , evalProgram
                 , evalProgramById
                 , getTrainingProblem
                 , getAnyTrainingProblem -- no size/ops limit
                 ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Vector as V
import Network.HTTP hiding (postRequest)
import qualified URLs
import Data.Text (Text)
import Data.Word
import Data.ByteString.Lazy.Char8 as BS hiding (map)
import Text.Printf (printf)

data Problem = Problem { problemId :: String
                       , problemSize :: Int
                       , operators :: [String]    -- TODO(vanya) This should be [Operator]
                       , solved :: Maybe Bool
                       , timeLeft :: Maybe Double
                       } deriving Show

data ProgramOrId = Program String | ID String deriving Show
data EvalRequest = EvalRequest { programOrId :: ProgramOrId
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

fromHex :: String -> Word64
fromHex = read

toHex :: Word64 -> String
toHex w = printf "0x%016X" w

instance FromJSON Problem where
  parseJSON (Object v) = Problem <$>
                         v .: "id" <*>
                         v .: "size" <*>
                         v .: "operators" <*>
                         v .:? "solved" <*>
                         v .:? "timeLeft"

(.=?) :: ToJSON a => [Pair] -> (Text, Maybe a) -> [Pair]
(.=?) ps (_, Nothing) = ps
(.=?) ps (name, Just v) = (name .= v) : ps

instance ToJSON Problem where
  toJSON p = object ([ "id" .= problemId p
                     , "size" .= problemSize p
                     , "operators" .= operators p
                     ]
                     .=? ("solved", solved p)
                     .=? ("timeLeft", timeLeft p)
                     )

instance FromJSON EvalResponse where
  parseJSON (Object v) = EvalResponse <$>
                         v .: "status" <*>
                         v .:? "outputs" <*>
                         v .:? "message"

instance ToJSON EvalResponse where
  toJSON p = object ([ "status" .= evalStatus p ]
                     .=? ("outputs", evalOutputs p)
                     .=? ("message", evalMessage p)
                     )

instance FromJSON Guess where
  parseJSON (Object v) = Guess <$>
                         v .: "id" <*>
                         v .: "program"

instance FromJSON GuessResponse where
  parseJSON (Object v) = GuessResponse <$>
                         v .: "status" <*>
                         (fmap (map fromHex) <$> (v .:? "values")) <*>
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
instance ToJSON EvalRequest where
  toJSON (EvalRequest pgmOrId vals) = 
    case pgmOrId of
      Program pgm -> object [ "program" .= pgm, "arguments" .= vals ]
      ID id       -> object [ "id" .= id, "arguments" .= vals ]

evalProgramById id vals  = postRequest URLs.eval (EvalRequest (ID id) vals)
evalProgram pgm vals     = postRequest URLs.eval (EvalRequest (Program pgm) vals)
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

