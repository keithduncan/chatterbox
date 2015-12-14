module Action.Ping (
  ping,
) where

import Data.Time.Clock.POSIX

import Control.Monad
import Control.Monad.IO.Class

import Web.Scotty
import Network.HTTP.Types

import Data.Map

ping :: ActionM ()
ping = do
  sec <- liftIO getPOSIXTime

  let ping = fromList [("status", "ok"), ("now", show sec)] :: Map String String

  status ok200
  json ping
