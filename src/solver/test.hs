import Test.SmallCheck.Series
import Types
import HsClient
import ServerAPI
import RandomBV
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LBS8

getProblem :: IO TrainingResponse
getProblem = do
  getTrainingProblem (Just 8) Nothing

progs :: [Exp]
progs = list 7 series

main = do
  p <- getProblem
  str <- evalProgramById (trainingId p) bvs
  putStrLn str

  {-
  print $ length $ filter (all (== 1234) . eval') progs
  where
    eval' p = map (\x -> eval x undefined undefined p) bvs
  -}
