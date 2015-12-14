{-# LANGUAGE OverloadedStrings #-}

module Routes (
  routes
) where

import Web.Scotty

import Action.Pong
import Action.Subscriptions
import Action.Topics

routes :: ScottyM ()
routes = do
  get "/_ping" pong

  -- Topics
  post "/topics/:topic" createMessage

  -- Subscriptions
  get "/subscriptions" listSubscriptions
  post "/subscriptions" createSubscription
  delete "/subscriptions" deleteSubscription
