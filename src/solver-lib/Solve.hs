module Solve (solve, solveExact, isFeasible) where

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

basicSolve :: Int -> [String] -> [Word64] -> [Word64] -> IO (Maybe ExpC)
basicSolve size operations inputs outputs = do
  let (alz, arz) = (minimum $ map numLeftZeros outputs, minimum $ map numRightZeros outputs)
  putStrLn ("RESTRICTION: allowed left/right zeros: " ++ show (alz, arz))
  let
    programs = generateRestrictedUpTo size operations (alz, arz)
    candidates = filterProgs inputs outputs $ filterByCached (head outputs) programs
  runPS candidates 4 (<= 3)

solve :: String -> Int -> [String] -> IO ()
solve pId size operations = do
  evalRes <- evalProgramById pId bvs
  case evalRes of
    EvalOK outputs -> loop bvs outputs
    EvalError msg -> error $ "evalProgramById returned error:" ++ show msg

  where
    loop inputs outputs = do
      putStrLn $
        "INOUTS to reproduce with client solve-exact: "
          ++ unwords operations ++ "\n"
          ++ show (inputs, outputs)
      first <-
        maybe (throwIO $ ErrorCall "Nothing has been found") return
        =<< basicSolve size operations inputs outputs

        -- mapM_ (putStrLn . ppProg) candidates
      print first
      gr <- guessProgram pId (ppProg first)
      case gr of
        Win -> print gr
        Mismatch input expected actual -> do
          putStrLn $ "Mismatch on " ++ show input ++ " : " ++ show actual ++ " instead of " ++ show expected
          loop (input:inputs) (expected:outputs)
        GuessError err -> error $ "guess error: " ++ err

onePos :: Word64 -> [Int]
onePos x = [i | i <- [0..63], x .&. (1 `shiftL` i) /= 0]

numRightZeros 0 = 64
numRightZeros x = minimum (onePos x)

numLeftZeros 0 = 64
numLeftZeros x = 63 - maximum (onePos x)

solveExact :: Int -> [String] -> [Word64] -> [Word64] -> IO ()
solveExact size operations inputs outputs = do
  result <- basicSolve size operations inputs outputs
  case result of
    Nothing -> putStrLn "Couldn't find any program matching conditions at all!"
    Just v -> print v

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

