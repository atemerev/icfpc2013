module Filter where

import Types
import Data.Maybe
import Control.Monad

filterProgs
  :: MonadPlus m
  => [Word64] -- inputs
  -> [Word64] -- outputs
  -> m ExpC -- all programs
  -> m ExpC -- programs that match
filterProgs ins outs progs =
  mfilter allInputsMatchAllOutputs progs
  where 
    allInputsMatchAllOutputs prog = 
      and $ zipWith
        (\inp outp -> eval inp undefined undefined prog == outp)
        ins outs

-- Filter generated list of expressions by checking if cached valued on known seed argument matches expectation
filterByCached :: MonadPlus m => Word64 -> m ExpC -> m ExpC
filterByCached expected progs = mfilter ((==expected).(fromMWord64 (error "no cached value in filterByCached")).cached) progs
