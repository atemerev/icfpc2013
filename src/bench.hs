import Types
import Filter
import Gen

import Criterion
import Criterion.Main

n = 10

main = defaultMain
  [ bench "not cached" $ nf (check . args) 10
  , bench "cached"     $ nf (checkCached .args) 10
  ]

check = length . filterProgs [9109764189119939331] [5]
checkCached = length . filterByCached 5
args n = generateRestricted n (words "if0 plus shl1 shr1 shr4 tfold xor")
