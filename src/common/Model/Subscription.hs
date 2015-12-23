module Model.Subscription (
  Subscription,
  subscription,
  getAdapter,
  getTopic,
  getExpiry,

  Adapter,
  Topic,
) where

import Network.URI (URI)
import Data.Time (UTCTime)

type Adapter = URI
type Topic = String

data Subscription = Subscription { getAdapter :: Adapter
                                 , getTopic :: Topic
                                 , getExpiry :: Maybe UTCTime
                                 }

subscription :: Adapter -> Topic -> Maybe UTCTime -> Subscription
subscription = Subscription
