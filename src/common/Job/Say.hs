{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Job.Say (
  SayJob(..),
  job,
) where

import System.Hworker
import System.IO (hPrint, stderr)

import Control.Monad (void)

import Model.Message
import Model.Subscription (Topic)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data SayJob = Say Topic Message deriving (Show, Generic)

instance ToJSON SayJob
instance FromJSON SayJob

instance Job () SayJob where
  job () (Say topic message) = do
    void $ hPrint stderr (show message ++ " -> " ++ topic)
    return Success
