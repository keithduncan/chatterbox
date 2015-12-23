module Model.Subscription (
  Subscription,
  subscription,
  getURI,
  getTopic,
  getExpiry,

  Adapter,
  Topic,
) where

import Network.URI (URI)
import Data.Time (UTCTime)

type Adapter = URI
type Topic = String

data Subscription = Subscription { getURI :: Adapter
                                 , getTopic :: Topic
                                 , getExpiry :: Maybe UTCTime
                                 }

subscription :: Adapter -> Topic -> Maybe UTCTime -> Subscription
subscription = Subscription
