module Database (
  Database,

  getDatabase,
) where

import Control.Monad (join)
import Control.Monad.Logger (runStderrLoggingT)

import Model.Subscription

import Environment (getEnvironmentURI)

import Network.URI (URI(..), URIAuth(..))

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)

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
             in bool defaultPort p (null p)

    defaultPort = "5432"

    stripLeading x xs = fromMaybe xs $ stripPrefix [x] xs
    stripTrailing x xs = maybe xs reverse $ stripPrefix [x] (reverse xs)
postgresConnectionInfo _ = error "invalid database URI"

createConnectionString :: [(String, String)] -> ConnectionString
createConnectionString l = encodeUtf8 . pack . unwords $ pair <$> l
  where
    pair :: (String, String) -> String
    pair (k, v) = concat [k, "=", v]
