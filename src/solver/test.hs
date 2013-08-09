import Test.SmallCheck.Series
import Types
import HttpClient
import ServerAPI
import RandomBV
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LBS8

getProblem :: IO Problem
getProblem = do
  str <- getTrainingProblem (Just 8) Nothing
  return $ fromMaybe (error "getProblem") $ decode $ LBS8.pack str

progs :: [Exp]
progs = list 7 series

main = do
  p <- getProblem
  str <- evalProgram (problemId p) bvs
  print str

  {-
  print $ length $ filter (all (== 1234) . eval') progs
  where
    eval' p = map (\x -> eval x undefined undefined p) bvs
  -}
