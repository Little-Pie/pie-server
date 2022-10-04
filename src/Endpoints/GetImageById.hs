{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetImageById where

import qualified DbQuery.Image as DB
import qualified Types.API.GetImageById as API
import Types.Entities.Image
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import qualified Data.ByteString.Char8 as BS
import Data.Aeson

getImageById :: Connection -> LBS.ByteString -> IO Response
getImageById conn body = case decode body :: Maybe API.GetImageByIdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let imageId' = API.imageId bodyParsed
    image' <- DB.getImageById conn imageId'
    case image' of
      [] -> pure $ responseBadRequest "There's no images with such id"
      (image:_) -> pure $ responseImage (BS.pack $ contentType image) (CLBS.pack $ base64Image image)
