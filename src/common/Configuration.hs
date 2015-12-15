{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration (
  Config(..),
  ConfigM(..),

  getConfig,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)

import Database
import Workqueue

data Config = Config { workqueue :: Workqueue
                     , database :: Database
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a
                            } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = Config <$>
  getWorkqueue <*>
  getDatabase
