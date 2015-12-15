{-# LANGUAGE DeriveGeneric #-}

module Model.Message (
  Message,
  message,
) where

import Data.Aeson
import GHC.Generics

data Message = Message deriving (Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
instance FromJSON Message

message :: Message
message = Message
