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
import Data.Time.Clock.POSIX (POSIXTime)

type Adapter = URI
type Topic = String

data Subscription = Subscription { getURI :: Adapter
                                 , getTopic :: Topic
                                 , getExpiry :: POSIXTime
                                 }

subscription :: Adapter -> Topic -> POSIXTime -> Subscription
subscription = Subscription
