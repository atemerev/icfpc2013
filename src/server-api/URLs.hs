module URLs where

mkURL path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=" ++ authToken
 where
   authToken = teamId ++ "vpsH1H"
   teamId = "0073xfoSG1mtZmZIyt70v4IPf1SnbJfcBVgkjRjj" 

myproblems = mkURL "myproblems"
train = mkURL "train"
eval = mkURL "eval"
guess = mkURL "guess"
status = mkURL "status"

