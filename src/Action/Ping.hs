module Action.Ping (
  ping,
) where

import System.Time

import Control.Monad

import Web.Scotty

import Data.Map

ping :: ActionM ()
ping = do
  (TOD sec _) <- liftIO getClockTime

  let status = fromList [("status", "ok"), ("now", show sec)] :: Map String String
  status ok200 >> json status
