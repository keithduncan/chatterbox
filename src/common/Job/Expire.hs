{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Job.Expire (
  ExpireJob(..),
  job,
) where

import System.Hworker

import Control.Monad (void)

import Model.Message
import Model.Subscription (Topic)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data ExpireJob = Expire deriving (Show, Generic)

instance ToJSON ExpireJob
instance FromJSON ExpireJob

instance Job () ExpireJob where
  job () Expire = do
    void $ print "expiring old subscriptions..."
    return Success
