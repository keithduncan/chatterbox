{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Schema where

import Database.Persist
import Database.Persist.Postgresql

import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

import Data.Text
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Subscription
  adapter   Text
  topic     Text
  expiry    UTCTime   Maybe

  AdapterTopic adapter topic

  deriving Show
|]
