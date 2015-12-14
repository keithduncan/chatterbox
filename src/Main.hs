
import System.Environment (lookupEnv)

import Web.Scotty

import Data.Maybe (maybe)

import Routes (routes)
import Configuration

main :: IO ()
main = getPort >>= runApplication

getPort :: IO Int
getPort = maybe 3000 read <$> lookupEnv "PORT"

runApplication :: Int -> IO ()
runApplication port = scotty port routes
