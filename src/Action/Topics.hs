module Action.Topics (
  createMessage,
) where

import Web.Scotty.Trans (ActionT)

import Configuration

import Data.Text.Lazy

createMessage :: ActionT Text ConfigM ()
createMessage = undefined
