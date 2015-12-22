module Environment (
  Environment(..),
  getEnvironment,

  getEnvironmentURI,
) where

import System.Environment (lookupEnv)

import Network.URI (URI, parseURI)

data Environment = Development | Production deriving (Eq)

instance Read Environment where
  readsPrec _ e = case e of
                   "development" -> [(Development, "")]
                   "production"  -> [(Production, "")]

getEnvironment :: IO Environment
getEnvironment = maybe Production read <$> lookupEnv "SCOTTY_ENV"

getEnvironmentURI :: String -> IO URI
getEnvironmentURI key = do
  uri <- lookupEnv key >>= maybe (fail $ key ++ " is missing from environment") return
  maybe (fail $ uri ++ " is an invalid uri") return $ parseURI uri
