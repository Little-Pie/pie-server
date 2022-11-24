{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.Users where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified DbQuery.User as DBU
import Helpers (lookup', responseOk)
import Network.Wai (Response)
import Text.Read (readMaybe)

getUsers :: [(BS.ByteString, Maybe BS.ByteString)] -> App Response
getUsers queryItems = do
  Environment {..} <- ask
  let (mbLimit, mbOffset) =
        ( (readMaybe . BS.unpack) =<< lookup' "limit" queryItems,
          (readMaybe . BS.unpack) =<< lookup' "offset" queryItems
        )
  let cfgLimit = limit
  let limit' =
        if cfgLimit < fromMaybe cfgLimit mbLimit
          then cfgLimit
          else fromMaybe cfgLimit mbLimit
  let offset' = fromMaybe offset mbOffset
  users <- liftIO $ DBU.showUsers conn limit' offset'
  responseOk $ encodePretty users
