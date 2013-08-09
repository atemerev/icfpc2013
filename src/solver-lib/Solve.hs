module Solve (solve, isFeasible) where 

import RandomBV (bvs)
import ServerAPI
import HsClient (evalProgramById, guessProgram)
import Gen (generateRestricted)
import Filter (filterProgs)
import PP (ppProg)
import Control.Exception (evaluate)
import System.Timeout


solve :: String -> Int -> [String] -> IO ()
solve progId size operations = do
  EvalOK outputs <- evalProgramById progId bvs -- TODO
  -- print outputs
  loop progId bvs outputs allProgs
  
  where 
    allProgs = generateRestricted size operations

    loop pId inputs outputs programs = do
      let
        candidates = filterProgs inputs outputs programs
        first = head candidates
        -- mapM_ (putStrLn . ppProg) candidates
      print first
      gr <- guessProgram pId (ppProg first)
      case gr of
        Win -> print gr
        Mismatch input expected actual -> do
          putStrLn $ "Mismatch on " ++ show input ++ " : " ++ show actual ++ " instead of " ++ show expected
          putStrLn $ "Continuing with " ++ show (length candidates) ++ " candidates left"
          loop pId (input:inputs) (expected:outputs) candidates
        GuessError err -> error $ "guess error: " ++ err


-- Check if it is feasible to solve this problem by brute-force within 'timeout' seconds
isFeasible :: Int -> Problem -> IO (Maybe (Problem, Int))
isFeasible tmout p
  | problemSize p >= 16 = return Nothing -- TODO
  | otherwise =
    timeout (tmout * 10^6) $ do
      let gen = generateRestricted (problemSize p) (operators p)
      let lgen = length gen
      evaluate lgen
      return (p, lgen)
      
