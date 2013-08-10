module Solve (solve, solve', solveExact, isFeasible) where 

import RandomBV (bvs)
import Types
import ServerAPI
import HsClient (evalProgramById, guessProgram)
import Gen (generateRestrictedUpTo)
import Filter (filterProgs, filterByCached)
import PP (ppProg)
import Control.Exception (evaluate)
import System.Timeout

solve :: String -> Int -> [String] -> IO ()
solve progId size operations = solve' progId (generateRestrictedUpTo size operations)

solveExact :: Int -> [String] -> [Word64] -> [Word64] -> IO ()
solveExact size operations inputs outputs = do
  let programs = generateRestrictedUpTo size operations
  let candidates = filterProgs inputs outputs programs
  if null candidates
    then putStrLn "Couldn't find any program matching conditions at all!"
    else print (head candidates)

solve' :: String -> [ExpC] -> IO ()
solve' progId allProgs = do
  evalRes <- evalProgramById progId bvs
  case evalRes of
    EvalOK outputs -> loop progId bvs outputs (filterByCached (head outputs) allProgs)
    EvalError msg -> error $ "evalProgramById returned error:" ++ show msg

  where 
    loop pId inputs outputs programs = do
      putStrLn $ "INOUTS to reproduce with client solve-exact: " ++ show (inputs, outputs)
      let
        candidates = filterProgs inputs outputs programs
        first = if null candidates
                then error "Couldn't find any program matching conditions at all!"
                else head candidates

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
      
