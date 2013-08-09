import Test.SmallCheck.Series
import Types
import Gen

main = print $ length $ list 10 (serProg noRestriction)
