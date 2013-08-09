{-# LANGUAGE OverloadedStrings #-}
module ServerAPI where

import Control.Applicative
import Data.Aeson

data Problem = Problem { problemId :: String
                       , problemSize :: Int
                       , operators :: [String]
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
