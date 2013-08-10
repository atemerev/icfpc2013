{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParSearch
  ( PS
  , runPS
  , MonadLevel(..)
  ) where

import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import ParSearch.Logic

class MonadPlus m => MonadLevel m where
  level :: m a -> m a

instance MonadLevel [] where
  level = id

data PSEnv = PSEnv
  { curLevel :: !Int
  , parallelP :: Int -> Bool
  , semaphore :: TVar Int
  }

newtype PS r a = PS { unPS :: ReaderT PSEnv (LogicT (Maybe r) IO) a }
  deriving (Functor, Applicative, Monad)

runPS
  :: PS a a
  -> Int -- number of threads
  -> (Int -> Bool) -- on which levels do we parallelize?
  -> IO (Maybe a)
runPS ps threads pp = do
  sem <- atomically $ newTVar threads
  searchIO $ runReaderT (unPS ps) (PSEnv 1 pp sem)

instance MonadLevel (PS r) where
  -- increase level
  level = PS . (local $ \env -> env { curLevel = curLevel env + 1 }) . unPS

instance Alternative (PS r) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (PS r) where
  mzero = PS mzero

  PS a `mplus` PS b = PS $ do
    env <- ask
    if parallelP env (curLevel env)
      then
        do
        lift $ searchInParallel (semaphore env) (runReaderT a env) (runReaderT b env)
      else a `mplus` b

searchInParallel
  :: TVar Int
  -> LogicT (Maybe r) IO a
  -> LogicT (Maybe r) IO a
  -> LogicT (Maybe r) IO a
searchInParallel sem la lb = LogicT $ \sk fk -> do
  yieldSemaphore sem $
    withAsync (searchIOBounded sem la sk fk) $ \asa ->
    withAsync (searchIOBounded sem lb sk fk) $ \asb -> do
    ei <- waitEither asa asb
    case ei of
      Left (Just v) -> return $ Just v
      Right (Just v) -> return $ Just v
      Left Nothing ->
        wait asb
      Right Nothing ->
        wait asa

searchIO :: LogicT (Maybe a) IO a -> IO (Maybe a)
searchIO a = runLogicT a (\x cont -> return $ Just x) (return Nothing)

searchIOBounded :: TVar Int -> LogicT r IO a -> (a -> IO r -> IO r) -> IO r -> IO r
searchIOBounded sem a sk fk = withSemaphore sem $
  evaluate =<< runLogicT a sk fk

withSemaphore sem a =
  bracket_ acquire release a
  where
    acquire = atomically $ do
      caps <- readTVar sem
      if caps > 0
        then
          writeTVar sem (caps - 1)
        else retry
    release = atomically $ modifyTVar' sem (+1)

yieldSemaphore sem a =
  bracket_ release acquire a
  where
    acquire = atomically $ modifyTVar' sem (subtract 1)
    release = atomically $ modifyTVar' sem (+1)
