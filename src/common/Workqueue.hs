{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workqueue (
  Workqueue(..),
  getWorkqueue,
) where

import System.Hworker

import Control.Monad (void)

import Model.Message (Message)
import Model.Subscription (Topic)

import Job.Say
import Job.Expire

type SayWorker = Hworker () SayJob
type ExpiryWorker = Hworker () ExpireJob

data Workqueue = Workqueue { getSayWorker :: SayWorker
                           , getExpiryWorker :: ExpiryWorker
                           }

getWorkqueue :: IO Workqueue
getWorkqueue = Workqueue <$>
  getSayHworker <*>
  getExpiryHworker

getSayHworker :: IO SayWorker
getSayHworker = create "say" ()

getExpiryHworker :: IO ExpiryWorker
getExpiryHworker = create "expire" ()
