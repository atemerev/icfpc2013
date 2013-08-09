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

data EvalResponse = EvalOK { evalOutputs :: [Word64] }
                  | EvalError { evalMessage :: Maybe String }
                  deriving Show

data Guess = Guess { guessProblemId :: String
                   , guessProgramText :: String -- TODO(vanya) should be Program
                   } deriving Show

data GuessResponse = Win
                   | Mismatch { guessInput :: Word64
                              , guessExpected :: Word64
                              , guessActual :: Word64
                              }
                   | GuessError { guessMessage :: String }
                   deriving Show

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
  parseJSON (Object v) = do
    status <- v .: "status"
    case status of
      "ok" -> EvalOK <$> map fromHex <$> (v .: "outputs")
      "error" -> EvalError <$> v .: "message"
      _ -> fail $ "Unknown EvalResponse status: " ++ status

instance ToJSON EvalResponse where
  toJSON (EvalOK os) = object [ "status" .= ("ok" :: String)
                              , "outputs" .= map toHex os
                              ]
  toJSON (EvalError msg) = object [ "status" .= ("error" :: String)
                                  , "message" .= msg
                                  ]

instance FromJSON Guess where
  parseJSON (Object v) = Guess <$>
                         v .: "id" <*>
                         v .: "program"

instance ToJSON Guess where
  toJSON p = object ([ "id" .= guessProblemId p
                     , "program" .= guessProgramText p
                     ])

instance FromJSON GuessResponse where
  parseJSON (Object v) = do
    status <- v .: "status"
    case status of
      "win" -> return Win
      "mismatch" -> do
        [input, expected, actual] <- v .: "values"
        return (Mismatch input expected actual)
      "error" -> GuessError <$> v .: "message"
      _ -> fail $ "Unknown GuessResponse status: " ++ status
                      

instance ToJSON GuessResponse where
  toJSON Win = object [ "status" .= ("win" :: String) ]
  toJSON (Mismatch i e a) = object [ "status" .= ("mismatch" :: String)
                                   , "values" .= [toHex i, toHex e, toHex a]
                                   ]
  toJSON (GuessError msg) = object [ "status" .= ("error" :: String)
                                   , "message" .= msg
                                   ]
