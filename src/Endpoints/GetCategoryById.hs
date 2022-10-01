{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetCategoryById where

import qualified Types.API.Id as API
import qualified DbQuery.Category as DB
import Types.Entities.Category
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson
import Helpers
import Network.Wai (Response)

getCategoryById :: Connection -> LBS.ByteString -> IO Response
getCategoryById conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let id = API.id bodyParsed
    category <- DB.getCategoryById conn id
    pure $ responseOk $ encodePretty category