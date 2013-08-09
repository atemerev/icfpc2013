{-# LANGUAGE OverloadedStrings #-}
module ServerAPI (getMyproblems) where

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

getMyproblems = do
  rsp <- Network.HTTP.simpleHTTP (getRequest URLs.myproblems)
         -- fetch document and return it (as a 'String'.)
  (2,0,0) <- getResponseCode rsp -- lamely ensure that we haven't got errors
  getResponseBody rsp
  
