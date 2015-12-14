module Action.Pong (
  pong,
) where

import Data.Time.Clock.POSIX

import Control.Monad
import Control.Monad.IO.Class

import Web.Scotty
import Network.HTTP.Types

import Data.Map

pong :: ActionM ()
pong = do
  sec <- liftIO getPOSIXTime

  let ping = fromList [("status", "ok"), ("now", show sec)] :: Map String String

  {-
    - [ ] Check Redis connection
    - [ ] Check Database connection
  -}

  status ok200
  json ping
