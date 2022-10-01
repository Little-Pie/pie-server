{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetPostById where

import qualified Types.API.Id as API
import qualified DbQuery.Post as DB
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson
import Helpers
import Network.Wai (Response)

getPostById :: Connection -> LBS.ByteString -> IO Response
getPostById conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let id = API.id bodyParsed
    post <- DB.getPostById conn id
    pure $ responseOk $ encodePretty post