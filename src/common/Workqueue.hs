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

type SayWorker = Hworker () SayJob

data Workqueue = Workqueue { getSayWorker :: SayWorker
                           }

getWorkqueue :: IO Workqueue
getWorkqueue = Workqueue <$>
  getSayHworker

getSayHworker :: IO SayWorker
getSayHworker = create "say" ()

enqueueSay :: Workqueue -> Topic -> Message -> IO ()
enqueueSay (Workqueue sayWorker) topic message = void $ queue sayWorker (Say topic message)
