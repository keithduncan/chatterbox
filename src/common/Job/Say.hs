{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Job.Say (
  SayJob(..),
  job,
) where

import System.Hworker
import System.IO (hPrint, stderr)

import Control.Monad (forM_)

import Model.Message
import Model.Subscription (Adapter, Topic, getAdapter)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Database

data SayJob = Say Topic Message deriving (Show, Generic)

instance ToJSON SayJob
instance FromJSON SayJob

instance Job () SayJob where
  job () (Say topic message) = do
    print ("delivering `" ++ show message ++ "` -> `" ++ topic ++ "`")

    subscriptions <- getDatabase >>= flip topicSubscriptions topic

    forM_ subscriptions $ \s ->
      print ("delivering `" ++ show message ++ "` -> `" ++ (show . getAdapter) s ++ "`")

    return Success

    where
      print = hPrint stderr
