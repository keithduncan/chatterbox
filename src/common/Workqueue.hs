{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Workqueue (
  Workqueue(..),
  getWorkqueue,

  enqueueSay,
) where

import System.Hworker
import System.Environment (lookupEnv)
import Environment (Environment(..), getEnvironment, getEnvironmentURI)
import System.IO (stderr, hPrint)

import Control.Monad (void, join)

import Model.Message (Message)
import Model.Subscription (Topic)

import qualified Database.Redis as R
import Network.URI

import Data.ByteString.Char8 (pack)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

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

  env <- getEnvironment

  Workqueue <$>
    getSayHworker env redis <*>
    getExpiryHworker env redis

getRedisConnection :: IO RedisConnection
getRedisConnection = do
  uri <- getEnvironmentURI "WORKQUEUE_URL"
  RedisConnection <$>
    (R.connect . redisConnectionInfo) uri

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

getSayHworker :: Environment -> RedisConnection -> IO SayWorker
getSayHworker = getHworker "say" ()

getExpiryHworker :: Environment -> RedisConnection -> IO ExpiryWorker
getExpiryHworker = getHworker "expire" ()

getHworker :: Job a b => Text -> a -> Environment -> RedisConnection -> IO (Hworker a b)
getHworker q s e c = createWith $ (defaultHworkerConfig q s) { hwconfigRedisConnectInfo = c
                                                             , hwconfigDebug = e == Development
                                                             , hwconfigLogger = hPrint stderr
                                                             }

enqueueSay :: Workqueue -> Topic -> Message -> IO ()
enqueueSay workqueue topic message = void $ queue (getSayWorker workqueue) (Say topic message)
