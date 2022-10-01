{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetUserById where

import qualified Types.API.Id as API
import qualified DbQuery.User as DB
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson
import Helpers
import Network.Wai (Response)

getUserById :: Connection -> LBS.ByteString -> IO Response
getUserById conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let id = API.id bodyParsed
    user <- DB.getUserById conn id
    pure $ responseOk $ encodePretty user