{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParSearch
  ( PS
  , runPS
  , MonadLevel(..)
  ) where

import Control.Monad.Logic
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

class MonadPlus m => MonadLevel m where
  level :: m a -> m a

instance MonadLevel [] where
  level = id

data PSEnv = PSEnv
  { curLevel :: !Int
  , parallelP :: Int -> Bool
  , semaphore :: TVar Int
  }

newtype PS a = PS { unPS :: ReaderT PSEnv (LogicT IO) a }
  deriving (Functor, Applicative, Monad)

runPS
  :: PS a
  -> Int -- number of threads
  -> (Int -> Bool) -- on which levels do we parallelize?
  -> IO (Maybe a)
runPS ps threads pp = do
  sem <- atomically $ newTVar threads
  searchIO $ runReaderT (unPS ps) (PSEnv 1 pp sem)

instance MonadLevel PS where
  -- increase level
  level = PS . (local $ \env -> env { curLevel = curLevel env + 1 }) . unPS

instance Alternative PS where
  empty = mzero
  (<|>) = mplus

instance MonadPlus PS where
  mzero = PS mzero

  PS a `mplus` PS b = PS $ do
    env <- ask
    if parallelP env (curLevel env)
      then
        lift $ searchInParallel (semaphore env) (runReaderT a env) (runReaderT b env)
      else a `mplus` b

searchInParallel
  :: TVar Int
  -> LogicT IO a
  -> LogicT IO a
  -> LogicT IO a
searchInParallel sem la lb = do
  r <- liftIO $
    withAsync (searchIOBounded sem la) $ \asa ->
    withAsync (searchIOBounded sem lb) $ \asb -> do
    ei <- waitEither asa asb
    case ei of
      Left (Just v) -> return $ Just v
      Right (Just v) -> return $ Just v
      Left Nothing -> wait asb
      Right Nothing -> wait asa

  maybe mzero return r

searchIO :: LogicT IO a -> IO (Maybe a)
searchIO a =
  evaluate =<< runLogicT a (\x cont -> return $ Just x) (return Nothing)

searchIOBounded :: TVar Int -> LogicT IO a -> IO (Maybe a)
searchIOBounded sem a = join . atomically $ do
  caps <- readTVar sem
  if caps > 0
    then do
      writeTVar sem (caps - 1)
      return $ searchIO a `finally` release
    else retry
  where
    release = atomically $ modifyTVar' sem (+1)
