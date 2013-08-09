module FileClient (getMyproblems, getMyproblemsHS, getUnsolvedHS, getUnsolved) where

import Data.ByteString.Lazy.Char8 as BS hiding (map, hPutStrLn, filter)
import ServerAPI
import Data.Aeson
import Data.Maybe

getMyproblemsHS :: String -> IO [Problem]
getMyproblemsHS fname = do
  pblms <- BS.readFile fname
  return $ fromMaybe (error "FileClient.getMyproblems decode failed") $ decode pblms

asString json = return (BS.unpack (encode json))

getMyproblems :: String -> IO String
getMyproblems fname = getMyproblemsHS fname >>= asString

getUnsolvedHS fname = do
  pblms <- getMyproblemsHS fname
  return $ filter (isNothing.solved) pblms
  
getUnsolved fname = getUnsolvedHS fname >>= asString
