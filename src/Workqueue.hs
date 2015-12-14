module Workqueue (
  Workqueue,

  getWorkqueue,
) where

import Model.Message
import Model.Subscription (Topic)

data Workqueue = Workqueue

getWorkqueue :: IO Workqueue
getWorkqueue = return Workqueue

enqueueSay :: Workqueue -> Topic -> Message -> IO ()
enqueueSay _ _ _ = return ()
