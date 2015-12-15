import System.Environment (lookupEnv)

import Web.Scotty.Trans (ScottyT, Options, scottyOptsT, settings, defaultHandler, middleware, json)
import Network.Wai (Response, Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Handler.Warp (Settings, setPort, defaultSettings)

import Data.Maybe (maybe)
import Data.Default (def)
import Data.Text.Lazy
import Data.Monoid
import Data.Aeson (Value (Null))
import Control.Monad.Reader (runReaderT)

import qualified Routes as App (routes)

import Environment
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
runApplication config = do
  options <- getOptions
  environment <- getEnvironment
  scottyOptsT options (run config) (routes environment >> App.routes)

run :: Config -> ConfigM Response -> IO Response
run config m = runReaderT (runConfigM m) config

routes :: Environment -> ScottyT Text ConfigM ()
routes env = do
  defaultHandler (\e -> json Null)
  middleware (logger env)

  where
    logger :: Environment -> Network.Wai.Middleware
    logger Development = logStdoutDev
    logger _           = logStdout
