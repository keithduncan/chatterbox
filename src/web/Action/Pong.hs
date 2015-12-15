module Action.Pong (
  pong,
) where

import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Monad.IO.Class (liftIO)

import Web.Scotty.Trans (ActionT, status, json)
import Network.HTTP.Types (ok200)

import Configuration

import Data.Map
import Data.Text.Lazy

pong :: ActionT Text ConfigM ()
pong = do
  sec <- liftIO getPOSIXTime

  let ping = fromList [("status", "ok"), ("now", show sec)] :: Map String String

  {-
    - [ ] Check Redis connection
    - [ ] Check Database connection
  -}

  status ok200
  json ping
