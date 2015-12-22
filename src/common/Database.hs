module Database (
  Database,

  getDatabase,
) where

import Control.Monad (join)

import Model.Subscription

import Environment (getEnvironmentURI)

import Network.URI (URI)

data Database = Database

getDatabase :: IO Database
getDatabase = return Database

getDatabaseURI :: IO URI
getDatabaseURI = getEnvironmentURI "DATABASE_URL"

subscriptionsWhere :: Database -> [(String, String)] -> [Subscription]
subscriptionsWhere = undefined
