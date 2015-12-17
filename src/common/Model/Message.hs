{-# LANGUAGE DeriveGeneric #-}

module Model.Message (
  Message,
  plainMessage,
) where

import Data.Aeson (ToJSON (toJSON), FromJSON, genericToJSON, defaultOptions)
import GHC.Generics (Generic)
import Data.Text (Text)

data Message = Plain { getContent :: Text
                     }
               deriving (Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
instance FromJSON Message

plainMessage :: Text -> Message
plainMessage = Plain
