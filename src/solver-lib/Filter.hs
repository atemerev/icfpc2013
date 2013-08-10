module Filter where

import Types

filterProgs
  :: [Word64] -- inputs
  -> [Word64] -- outputs
  -> [ExpC] -- all programs
  -> [ExpC] -- programs that match
filterProgs ins outs progs =
  filter allInputsMatchAllOutputs progs
  where 
    allInputsMatchAllOutputs prog = 
      and $ zipWith
        (\inp outp -> eval inp undefined undefined prog == outp)
        ins outs

-- Filter generated list of expressions by checking if cached valued on known seed argument matches expectation
filterByCached :: Word64 -> [ExpC] -> [ExpC]
filterByCached expected progs = filter ((==expected).cached) progs
