module Routes (
  routes
) where

import Action.Ping

routes :: ScottyM ()
routes = do
  get "/_ping" ping
