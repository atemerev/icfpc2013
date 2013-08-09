module FileClient (getMyproblems, getMyproblemsHS, getUnsolvedHS, getUnsolved, filterByIds) where

import Data.ByteString.Lazy.Char8 as BS hiding (map, hPutStrLn, filter)
import ServerAPI
import Data.Aeson
import Data.Maybe
import Control.Applicative
import qualified Data.Set as S

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

filterByIds problemsFile idsFile = do
  problems <- getUnsolvedHS problemsFile
  ids <- S.fromList . Prelude.lines <$> Prelude.readFile idsFile
  let rest = filter (not.(`S.member` ids).problemId) problems
  asString rest
