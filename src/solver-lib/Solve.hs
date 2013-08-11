module Solve (solve, solve', solveExact, isFeasible) where 

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
import Data.Bits

solve :: String -> Int -> [String] -> IO ()
solve progId size operations = solve' progId (generateRestrictedUpTo size operations (64, 64))

onePos :: Word64 -> [Int]
onePos x = [i | i <- [0..63], x .&. (1 `shiftL` i) /= 0]

numRightZeros 0 = 64
numRightZeros x = minimum (onePos x)

numLeftZeros 0 = 64
numLeftZeros x = 63 - maximum (onePos x)

solveExact :: Int -> [String] -> [Word64] -> [Word64] -> IO ()
solveExact size operations inputs outputs = do
  let (alz, arz) = (minimum $ map numLeftZeros outputs, minimum $ map numRightZeros outputs)
  let programs = generateRestrictedUpTo size operations (alz, arz)
  let candidates = filterProgs inputs outputs programs
  if null candidates
    then putStrLn "Couldn't find any program matching conditions at all!"
    else print (head candidates)

solve' :: String -> PS ExpC ExpC -> IO ()
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
      let gen = generateRestrictedUpTo (problemSize p) (operators p) (64, 64)
      let lgen = length gen
      evaluate lgen
      return ()
      
