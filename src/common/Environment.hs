module Environment (
  Environment(..),
  getEnvironment,
) where

import System.Environment (lookupEnv)

data Environment = Development | Production deriving (Eq)

instance Read Environment where
  readsPrec _ e = case e of
                   "development" -> [(Development, "")]
                   "production"  -> [(Production, "")]

getEnvironment :: IO Environment
getEnvironment = maybe Production read <$> lookupEnv "SCOTTY_ENV"
