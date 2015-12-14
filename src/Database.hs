module Database (
  Database,

  getDatabase,
) where

data Database = Database

getDatabase :: IO Database
getDatabase = return Database
