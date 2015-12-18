{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workqueue (
  Workqueue(..),
  getWorkqueue,
) where

import System.Hworker
import System.Environment (lookupEnv)

import Control.Monad (void, join)

import Model.Message (Message)
import Model.Subscription (Topic)

import qualified Database.Redis as R
import Network.URI

import Data.ByteString.Char8 (pack)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Job.Say
import Job.Expire

type SayWorker = Hworker () SayJob
type ExpiryWorker = Hworker () ExpireJob

data Workqueue = Workqueue { getSayWorker :: SayWorker
                           , getExpiryWorker :: ExpiryWorker
                           }

getWorkqueue :: IO Workqueue
getWorkqueue = do
  redis <- getRedisConnection

  Workqueue <$>
    getSayHworker redis <*>
    getExpiryHworker redis

getRedisConnection :: IO RedisConnection
getRedisConnection = do
  uri <- join . (parseURI <$>) <$> lookupEnv "WORKQUEUE_REDIS"
  RedisConnection <$> R.connect (case uri of
    Nothing -> R.defaultConnectInfo
    Just c  -> redisConnectionInfo c)

redisConnectionInfo :: URI -> R.ConnectInfo
redisConnectionInfo (URI "redis:" (Just (URIAuth auth regname port)) path _ _) =
  R.defaultConnectInfo { R.connectHost = regname'
                       , R.connectPort = portNumber
                       , R.connectAuth = authentication
                       , R.connectDatabase = database
                       }

  where
    regname' = takeWhile (/=']') . dropWhile (=='[') $ regname

    portNumber = if null port
                 then R.connectPort R.defaultConnectInfo
                 else R.PortNumber . fromInteger . read $ stripLeading ':' port

    authentication = case break (==':') (stripTrailing '@' auth) of
                       (u, ':':p) -> Just (pack p)
                       _          -> Nothing

    database = let db = stripLeading '/' path
                in if null db
                   then R.connectDatabase R.defaultConnectInfo
                   else read db

    stripLeading x xs = fromMaybe xs $ stripPrefix [x] xs
    stripTrailing x xs = maybe xs reverse $ stripPrefix [x] (reverse xs)
redisConnectionInfo uri = error $ "invalid URI " ++ show uri

getSayHworker :: RedisConnection -> IO SayWorker
getSayHworker c = createWith $ (defaultHworkerConfig "say" ()) { hwconfigRedisConnectInfo = c }

getExpiryHworker :: RedisConnection -> IO ExpiryWorker
getExpiryHworker c = createWith $ (defaultHworkerConfig "expire" ()) { hwconfigRedisConnectInfo = c }
