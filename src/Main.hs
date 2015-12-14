
import System.Environment

import Web.Scotty

import Data.Maybe
import Routes

main :: IO ()
main = getPort >>= runApplication

getPort :: IO Int
getPort = maybe 3000 read <$> lookupEnv "PORT"

runApplication :: Int -> IO ()
runApplication port = scotty port routes
