module Main where

import Blaze.ByteString.Builder (copyByteString)
--import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Response(..), pathInfo)
import Network.Wai.Handler.Warp (run)
import System.IO (putStrLn)

import ServerAPI

main = do
  let port = 8888
  putStrLn $ "FakeServer is listening on port " ++ show port
  run port fakeServer

fakeServer :: Application
fakeServer req = return $
  case pathInfo req of
    ["eval"] -> eval
    _ -> notFound

notFound = ResponseBuilder status404 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString ["not found"]
eval = ResponseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString ["ok"]

