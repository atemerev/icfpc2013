{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
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
import qualified Data.IntSet as S

import GHC.Conc
import ParSearch.Logic
--import Debug.Trace
import Text.Printf

{-# INLINE traceIO #-}
traceIO :: String -> IO ()
traceIO _ = return ()

class MonadPlus m => MonadLevel m where
  level :: m a -> m a

instance MonadLevel [] where
  level = id

data PSEnvOuter = PSEnvOuter
  { curLevel :: !Int
  , parallelP :: Int -> Bool
  }
data PSEnvInner = PSEnvInner
  { freePriority :: TVar Int -- feel free to fetch this priority and increment the counter
  , ourPriority :: Int
  , executingPriority :: TVar (Int, S.IntSet)
    -- priorities <= this may execute right now
    -- plus a set holding currently executing priorities
  }

newtype PS r a = PS { unPS :: ReaderT PSEnvOuter (LogicT (Maybe r) (ReaderT PSEnvInner IO)) a }
  deriving (Functor, Applicative, Monad)

runPS
  :: PS a a
  -> Int -- number of threads
  -> (Int -> Bool) -- on which levels do we parallelize?
  -> IO (Maybe a)
runPS ps threads pp = do
  ep <- atomically $ newTVar (threads, S.singleton 1)
  fp <- atomically $ newTVar 2
  runReaderT (searchIO $ runReaderT (unPS ps) (PSEnvOuter 1 pp)) (PSEnvInner fp 1 ep)

instance MonadLevel (PS r) where
  -- increase level
  level = PS . (local $ \env -> env { curLevel = curLevel env + 1 }) . unPS

instance Alternative (PS r) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (PS r) where
  mzero = PS mzero

  a `mplus` b = do
    env <- PS ask
    if parallelP env (curLevel env)
      then
        searchInParallel a b
      else PS $ unPS a `mplus` unPS b

searchInParallel
  :: PS r a
  -> PS r a
  -> PS r a
searchInParallel la lb =
  PS $
  ReaderT $ \envOuter ->
  LogicT $ \sk fk ->
  ReaderT $ \envInner -> do
    { let toIO a = runReaderT (mkThread $ rmLogic envOuter sk (return Nothing {-XXX-}) a) envInner
    ; runReaderT yieldPriority envInner
    ; withAsync (toIO la) $ \asa ->
      withAsync (toIO lb) $ \asb -> do
      ei <- waitEither asa asb
      case ei of
        Left (Just v) -> return $ Just v
        Right (Just v) -> return $ Just v
        _ -> do
          r <-
            wait
              (case ei of
                Left Nothing -> asb
                Right Nothing -> asa)
          case r of
            Just v -> return $ Just v
            Nothing -> runReaderT (mkThread fk) envInner
    }

-- remove the Logic layer from the stack
-- rmLogic :: PS a -> ReaderT PSEnv IO a
rmLogic envOuter sk fk ps = runLogicT (runReaderT (unPS ps) envOuter) sk fk

-- searchIO :: LogicT (Maybe a) IO a -> IO (Maybe a)
searchIO :: Monad m => LogicT (Maybe a) m a -> m (Maybe a)
searchIO a = runLogicT a (\x cont -> return $ Just x) (return Nothing)

fetchPriority :: (MonadIO m, MonadReader PSEnvInner m) => m Int
fetchPriority = do
  pvar <- asks freePriority
  liftIO $ atomically $ do
    p <- readTVar pvar
    writeTVar pvar $! p+1
    return p

yieldPriority :: (MonadIO m, MonadReader PSEnvInner m) => m ()
yieldPriority = do
  our <- asks ourPriority
  ep <- asks executingPriority
  liftIO $ join $ atomically $ do
    (cur, set) <- readTVar ep
    if our `S.member` set
      then do
        let set' = S.delete our set
        writeTVar ep (cur+1, set')
        return $
          traceIO $ printf "Yield priority from %d: now (%d, %s)" our (cur+1) (show $ S.toList set')
      else
        return $ traceIO $ printf "%d: yieldPriority: not on the list" our

mkThread :: ReaderT PSEnvInner IO a -> ReaderT PSEnvInner IO a
mkThread a = ReaderT $ \penv -> do
  myp <- runReaderT fetchPriority penv
  let
    parent = ourPriority penv
    env = penv { ourPriority = myp }
  (do
    traceIO $ printf "Got priority %d (parent = %d)" myp parent
    myThreadId >>= \i -> labelThread i (show myp)
      -- wait for execution
    let ep = executingPriority env
    traceIO $ printf "%d: now sleeping" myp
    (join $ atomically $ do
      (curp, set) <- readTVar ep
      if myp <= curp
        then do
          let set' = S.insert myp set
          writeTVar ep (curp, set')
          return $
            traceIO $ printf "Priority %d: start execution %s" myp (show (curp,S.toList set'))
        else retry)
        `catch` \e -> do traceIO (printf "%d: caught %s" (show (e :: SomeException))); throwIO e

    -- execute
    traceIO (printf "%d: about to start" myp)
    r <- (evaluate =<< runReaderT a env)

    return r) `finally` (traceIO (printf "%d: finally finish" myp) >> runReaderT yieldPriority env)
