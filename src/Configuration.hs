{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration (
  Config(..),

  getConfig,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Functor

data Workqueue = Workqueue

data Database = Database

data Config = Config { workqueue :: Workqueue
                     , database :: Database
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a
                            } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = Config <$>
  getWorkqueue <*>
  getDatabase

getWorkqueue = return Workqueue

getDatabase = return Database