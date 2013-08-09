module Solve (solve) where 

import RandomBV (bvs)
import ServerAPI
import HsClient (evalProgramById, guessProgram)
import Gen (generateRestricted)
import Filter (filterProgs)
import PP (ppProg)

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
