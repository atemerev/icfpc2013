module Solve (solve, solve', isFeasible) where 

import RandomBV (bvs)
import Types
import ServerAPI
import HsClient (evalProgramById, guessProgram)
import Gen (generateRestrictedUpTo)
import Filter (filterProgs, filterByCached)
import PP (ppProg)
import ParSearch
import Control.Exception
import System.Timeout

solve :: String -> Int -> [String] -> IO ()
solve progId size operations = solve' progId (generateRestrictedUpTo size operations)

solve' :: String -> PS ExpC ExpC -> IO ()
solve' progId allProgs = do
  evalRes <- evalProgramById progId bvs
  case evalRes of
    EvalOK outputs -> loop progId bvs outputs (filterByCached (head outputs) allProgs)
    EvalError msg -> error $ "evalProgramById returned error:" ++ show msg

  where 
    loop pId inputs outputs programs = do
      let
        candidates = filterProgs inputs outputs programs
      first <-
        maybe (throwIO $ ErrorCall "Nothing has been found") return
        =<< runPS candidates 4 (<= 3)
        -- mapM_ (putStrLn . ppProg) candidates
      print first
      gr <- guessProgram pId (ppProg first)
      case gr of
        Win -> print gr
        Mismatch input expected actual -> do
          putStrLn $ "Mismatch on " ++ show input ++ " : " ++ show actual ++ " instead of " ++ show expected
          loop pId (input:inputs) (expected:outputs) candidates
        GuessError err -> error $ "guess error: " ++ err


-- Check if it is feasible to solve this problem by brute-force within 'timeout' seconds
isFeasible :: Int -> Problem -> IO (Maybe ())
isFeasible tmout p 
  | problemSize p >= 16 = return Nothing
  | otherwise =
    timeout (tmout * 10^6) $ do
      let gen = generateRestrictedUpTo (problemSize p) (operators p)
      let lgen = length gen
      evaluate lgen
      return ()
      
