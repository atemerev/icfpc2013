module Filter where

import Types

filterProgs
  :: [Word64] -- inputs
  -> [Word64] -- outputs
  -> [Exp] -- all programs
  -> [Exp] -- programs that match
filterProgs ins outs =
  filter $ \prog -> and $
    zipWith
      (\inp outp -> eval inp 0 0 prog == outp)
      ins outs
