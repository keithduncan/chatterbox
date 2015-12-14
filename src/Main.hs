
import System.Environment (lookupEnv)

import Web.Scotty.Trans
import Network.Wai (Response)
import Network.Wai.Handler.Warp (Settings, setPort, defaultSettings)

import Data.Maybe (maybe)
import Data.Default (def)
import Control.Monad.Reader (runReaderT)

import Routes (routes)
import Configuration

main :: IO ()
main = getConfig >>= runApplication

getPort :: IO Int
getPort = maybe 3000 read <$> lookupEnv "PORT"

getOptions :: IO Options
getOptions = getSettings >>= \s -> return def { settings = s }

getSettings :: IO Settings
getSettings = do
  port <- getPort
  return $ setPort port defaultSettings

runApplication :: Config -> IO ()
runApplication config = getOptions >>= (\options -> scottyOptsT options run routes)
  where
    run :: ConfigM Response -> IO Response
    run m = runReaderT (runConfigM m) config
