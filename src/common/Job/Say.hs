{-# LANGUAGE DeriveGeneric #-}

module Job.Say (
) where

import Model.Message
import Model.Subscription (Topic)

import Data.Aeson
import GHC.Generics

data SayJob = Say Topic Message deriving (Show, Generic)

instance ToJSON SayJob
instance FromJSON SayJob
