{-# LANGUAGE OverloadedStrings #-}
module ServerAPI where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Word
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
                   , guessProgramText :: String -- TODO(vanya) should be Program
                   } deriving Show

data GuessResponse = GuessResponse { guessStatus :: String
                                   , guessValues :: Maybe [Word64]
                                   , guessMessage :: Maybe String
                                   } deriving Show

data OpLimit = NoFolds | Fold | TFold deriving Show
data TrainingRequest = TrainingRequest { size :: Maybe Int
                                       , opLimit :: Maybe OpLimit
                                       } deriving Show

array lst = Array $ V.fromList lst
instance ToJSON OpLimit where
  toJSON NoFolds = array []
  toJSON Fold    = array ["fold"]
  toJSON TFold   = array ["tfold"]
  
instance ToJSON TrainingRequest where
  toJSON (TrainingRequest sz op) = object [ "size" .= sz, "operators" .= op ]
  
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

instance ToJSON EvalRequest where
  toJSON (EvalRequest pgmOrId vals) = 
    case pgmOrId of
      Program pgm -> object [ "program" .= pgm, "arguments" .= vals ]
      ID id       -> object [ "id" .= id, "arguments" .= vals ]

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

instance ToJSON Guess where
  toJSON p = object ([ "id" .= guessProblemId p
                     , "program" .= guessProgramText p
                     ])

instance FromJSON GuessResponse where
  parseJSON (Object v) = GuessResponse <$>
                         v .: "status" <*>
                         (fmap (map fromHex) <$> (v .:? "values")) <*>
                         v .:? "message"

instance ToJSON GuessResponse where
  toJSON p = object ([ "status" .= guessStatus p ]
                     .=? ("values", guessValues p)
                     .=? ("message", guessMessage p)
                     )

