module Database (
  Database,

  getDatabase,
) where

import Model.Subscription

data Database = Database

getDatabase :: IO Database
getDatabase = return Database

subscriptionsWhere :: Database -> [(String, String)] -> [Subscription]
subscriptionsWhere = undefined
