{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workqueue (
  Workqueue,

  getWorkqueue,

  enqueueSay,
) where

import System.Hworker

import Control.Monad (void)

import Model.Message
import Model.Subscription (Topic)

import Job.Say

type SayWorker = Hworker ()

data Workqueue = Workqueue { getSayWorker :: Hworker () SayJob
                           }

getWorkqueue :: IO Workqueue
getWorkqueue = Workqueue <$>
  getSayHworker

getSayHworker :: IO (Hworker () SayJob)
getSayHworker = create "say" ()

enqueueSay :: Workqueue -> Topic -> Message -> IO ()
enqueueSay (Workqueue sayWorker) topic message = void $ queue sayWorker (Say topic message)
