import System.Hworker

import Workqueue

import Control.Monad
import Control.Concurrent

main :: IO ()
main = getWorkqueue >>= runWorkqueue

runWorkqueue :: Workqueue -> IO ()
runWorkqueue workqueue = do
  5 `workers` getSayWorker workqueue
  1 `workers` getExpiryWorker workqueue

  forever (threadDelay 1000000)

workers :: Job a b => Int -> Hworker a b -> IO [ThreadId]
workers count w = do
  monitorId <- forkIO (monitor w)
  workerIds <- replicateM count (forkIO (worker w))
  return $ monitorId:workerIds
