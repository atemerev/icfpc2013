-- import Types
-- import Gen
import HsClient
import ServerAPI
-- import Filter
-- import PP
-- import Data.Maybe
import Solve (solve)

targetSize = 12

getProblem :: IO TrainingResponse
getProblem = do
  getTrainingProblem (Just targetSize) Nothing

main = do
  p <- getProblem
  let progId = (trainingId p)
  print p
  solve (trainingId p) targetSize (trainingOps p)
