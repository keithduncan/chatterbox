module Action.Subscriptions (
  listSubscriptions,
  createSubscription,
  deleteSubscription,
) where

import Web.Scotty.Trans (ActionT, json)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (asks)

import Data.Aeson (Value (Null))
import Data.Text.Lazy

import Configuration
import Database

listSubscriptions :: ActionT Text ConfigM ()
listSubscriptions = do
  database <- lift (asks database)

  json Null

createSubscription :: ActionT Text ConfigM ()
createSubscription = undefined

deleteSubscription :: ActionT Text ConfigM ()
deleteSubscription = undefined
