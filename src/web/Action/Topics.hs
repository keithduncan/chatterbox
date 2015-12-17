{-# LANGUAGE OverloadedStrings #-}

module Action.Topics (
  createMessage,
) where

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader

import Web.Scotty.Trans (ActionT, param, header, body, status, json)
import Network.HTTP.Types (badRequest400, ok200, unsupportedMediaType415, accepted202)

import Configuration
import Model.Message (Message, plainMessage)
import Workqueue (enqueueSay)

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Map as Map
import Data.String
import Data.ByteString.Lazy
import Data.Aeson (Value (Null))

createMessage :: ActionT Text ConfigM ()
createMessage = do
  topic <- param "topic" :: ActionT Text ConfigM Text
  if T.null topic
  then do
    status badRequest400
    jsonError "missing topic parameter"
  else do
    contentType <- header "Content-Type"
    let decoder = decodeContentType <$> contentType

    b <- body
    let decoded = join (decoder <*> return b)

    case decoded of
      Nothing -> do
        status unsupportedMediaType415
        jsonError "unsupported content-type"
      Just m  -> do
        workqueue <- lift (asks workqueue)

        liftIO (enqueueSay workqueue (T.unpack topic) m)

        status accepted202
        json Null

decodeContentType :: T.Text -> ByteString -> Maybe Message
decodeContentType c b
  -- TODO check just the MIME type, ignore the MIME parameters
  | CI.mk c == "text/plain" = Just (plainMessage $ E.decodeUtf8 b)
decodeContentType _ _ = Nothing

jsonError :: String -> ActionT T.Text ConfigM ()
jsonError t = let err = Map.fromList [("message", t)] :: Map.Map String String
               in json err
