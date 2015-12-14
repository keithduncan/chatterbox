{-# LANGUAGE OverloadedStrings #-}

module Routes (
  routes
) where

import Web.Scotty

import Action.Pong

routes :: ScottyM ()
routes = do
  get "/_ping" pong
