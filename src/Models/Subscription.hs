module Models.Subscription (
  Subscription,
  subscription,
  getURI,
  getTopic,
  getExpiry,

  Topic,
) where

import Network.URI (URI)
import Data.Time.Clock.POSIX (POSIXTime)

type Topic = String

data Subscription = Subscription { getURI :: URI
                                 , getTopic :: Topic
                                 , getExpiry :: POSIXTime
                                 }

subscription :: URI -> Topic -> POSIXTime -> Subscription
subscription = Subscription
