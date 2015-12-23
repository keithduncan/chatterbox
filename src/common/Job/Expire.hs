{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Job.Expire (
  ExpireJob(..),
  job,
) where

import System.Hworker

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Database

data ExpireJob = Expire deriving (Show, Generic)

instance ToJSON ExpireJob
instance FromJSON ExpireJob

instance Job () ExpireJob where
  job () Expire = do
    print "expiring old subscriptions..."

    getDatabase >>= deleteExpiredSubscriptions

    print "old subscriptions expired"

    return Success
