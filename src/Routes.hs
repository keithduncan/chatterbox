{-# LANGUAGE OverloadedStrings #-}

module Routes (
  routes
) where

import Web.Scotty.Trans (ScottyT, get, post, delete, status, json, notFound, defaultHandler)
import Network.HTTP.Types (status404)

import Data.Aeson (Value (Null))
import Data.Text.Lazy

import Configuration

import Action.Pong
import Action.Subscriptions
import Action.Topics

routes :: ScottyT Text ConfigM ()
routes = do
  get "/_ping" pong

  -- Topics
  post "/topics/:topic" createMessage

  -- Subscriptions
  get "/subscriptions" listSubscriptions
  post "/subscriptions" createSubscription
  delete "/subscriptions" deleteSubscription

  notFound (status status404 >> json Null)
