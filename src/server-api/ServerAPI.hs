{-# LANGUAGE OverloadedStrings #-}
module ServerAPI ( getMyproblems
                 , getStatus
                 ) where

import Control.Applicative
import Data.Aeson
import Network.HTTP
import qualified URLs

data Problem = Problem { problemId :: String
                       , problemSize :: Int
                       , operators :: [String]    -- TODO(vanya) This should be [Operator]
                       , isSolved :: Maybe Bool
                       , timeLeft :: Maybe Double
                       } deriving Show

instance FromJSON Problem where
  parseJSON (Object v) = Problem <$>
                         v .: "id" <*>
                         v .: "size" <*>
                         v .: "operators" <*>
                         v .:? "solved" <*>
                         v .:? "timeLeft"

arglessRequest url = do
  rsp <- Network.HTTP.simpleHTTP (getRequest url)
         -- fetch document and return it (as a 'String'.)
  (2,0,0) <- getResponseCode rsp -- lamely ensure that we haven't got errors
  getResponseBody rsp

-- 1. Getting problems
getMyproblems = arglessRequest URLs.myproblems
  
-- 2. Evaluating programs
-- 3. Submitting guesses
-- 4. Training
-- 5. Status
getStatus = arglessRequest URLs.status
