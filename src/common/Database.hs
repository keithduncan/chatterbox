{-# LANGUAGE FlexibleContexts #-}

module Database (
  Database,

  getDatabase,

  Database.runMigration,
  Database.printMigration,

  adapterSubscriptions,
  topicSubscriptions,

  deleteExpiredSubscriptions,
) where

import Control.Monad (join, void)
import Control.Monad.Logger (runStderrLoggingT)

import qualified Schema as S
import Model.Subscription

import Environment (getEnvironmentURI)

import Network.URI (URI(..), URIAuth(..), parseURI)

import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bool (bool)
import Data.Time.Clock (getCurrentTime)

import Database.Persist as DB
import Database.Persist.Postgresql as DB

data Database = Database { getConnectionPool :: ConnectionPool
                         }

getDatabase :: IO Database
getDatabase = Database <$> connectionPool

connectionPool :: IO ConnectionPool
connectionPool = do
  connectionString <- createConnectionString . postgresConnectionInfo <$> getDatabaseURI
  runStderrLoggingT (createPostgresqlPool connectionString 5)

getDatabaseURI :: IO URI
getDatabaseURI = getEnvironmentURI "DATABASE_URL"

postgresConnectionInfo :: URI -> [(String, String)]
postgresConnectionInfo (URI "postgres:" (Just (URIAuth auth regname port)) path _ _) =
  [
    ("user", user)
  , ("password", password)

  , ("host", regname)
  , ("port", port')

  , ("dbname", stripLeading '/' path)
  ]

  where
    (user, password) = case break (==':') (stripTrailing '@' auth) of
                         (u, ':':p) -> (u, p)
                         _          -> ("", "")

    port' = let p = stripLeading ':' port
             in bool p defaultPort (null p)

    defaultPort = "5432"

    stripLeading x xs = fromMaybe xs $ stripPrefix [x] xs
    stripTrailing x xs = maybe xs reverse $ stripPrefix [x] (reverse xs)
postgresConnectionInfo _ = error "invalid database URI"

createConnectionString :: [(String, String)] -> ConnectionString
createConnectionString l = encodeUtf8 . pack . unwords $ pair <$> l
  where
    pair :: (String, String) -> String
    pair (k, v) = concat [k, "=", v]

adapterSubscriptions :: Database -> Adapter -> IO [Subscription]
adapterSubscriptions db adapter = viewModels <$> sql db (selectList [S.SubscriptionAdapter ==. (pack . show) adapter] [])

topicSubscriptions :: Database -> Topic -> IO [Subscription]
topicSubscriptions db topic = viewModels <$> sql db (selectList [S.SubscriptionTopic ==. pack topic] [])

viewModels :: [Entity S.Subscription] -> [Subscription]
viewModels = catMaybes . (viewModel . entityVal <$>)

viewModel :: S.Subscription -> Maybe Subscription
viewModel s = (\a -> subscription a topic expiry) <$> adapter
  where
    adapter = (parseURI . unpack . S.subscriptionAdapter) s
    topic = (unpack . S.subscriptionTopic) s
    expiry = S.subscriptionExpiry s

-- TODO check this doesn't delete entries with a NULL expiry time
deleteExpiredSubscriptions :: Database -> IO ()
deleteExpiredSubscriptions db = do
  now <- getCurrentTime
  void $ sql db (deleteWhere [S.SubscriptionExpiry <. Just now])

sql db = flip runSqlPool (getConnectionPool db)

runMigration :: Database -> IO ()
runMigration db = flip runSqlPersistMPool (getConnectionPool db) $ DB.runMigration S.migrateAll

printMigration :: Database -> IO ()
printMigration db = flip runSqlPersistMPool (getConnectionPool db) $ DB.printMigration S.migrateAll
