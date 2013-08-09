import Test.SmallCheck.Series
import Types
import HsClient
import ServerAPI
import RandomBV
import Filter
import PP
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LBS8

getProblem :: IO TrainingResponse
getProblem = do
  getTrainingProblem (Just 8) Nothing

progs :: [Exp]
progs = list 8 series

main = do
  p <- getProblem
  let progId = (trainingId p)
  EvalOK outputs <- evalProgramById progId bvs
  print outputs

  let
    candidates = filterProgs bvs outputs progs
    first = head candidates -- XXX
  -- mapM_ (putStrLn . ppProg) candidates
  print first

  gr <- guessProgram progId (ppProg first)
  print gr
  {-
  print $ length $ filter (all (== 1234) . eval') progs
  where
    eval' p = map (\x -> eval x undefined undefined p) bvs
  -}
