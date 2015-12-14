module Workqueue (
  Workqueue,

  getWorkqueue,
) where

data Workqueue = Workqueue

getWorkqueue :: IO Workqueue
getWorkqueue = return Workqueue
