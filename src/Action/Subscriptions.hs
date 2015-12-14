module Action.Subscriptions (
  listSubscriptions,
  createSubscription,
  deleteSubscription,
) where

import Web.Scotty.Trans (ActionT)

import Configuration

import Data.Text.Lazy

listSubscriptions :: ActionT Text ConfigM ()
listSubscriptions = undefined

createSubscription :: ActionT Text ConfigM ()
createSubscription = undefined

deleteSubscription :: ActionT Text ConfigM ()
deleteSubscription = undefined
