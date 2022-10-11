{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetImageById where

import qualified DbQuery.Image as DB
import qualified Types.API.GetImageById as API
import Types.Entities.Image
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import qualified Data.ByteString.Base64 as BASE

getImageById :: Connection -> Int -> IO Response
getImageById conn imageId = do
    dbImage <- DB.getImageById conn imageId
    case dbImage of
      [] -> pure $ responseBadRequest "There's no images with such id"
      (image:_) -> case BASE.decode $ BS.pack $ base64Image image of
        Left err -> pure $ responseBadRequest "Couldn't decode from base64"
        Right decodedImage -> pure $ responseImage (BS.pack $ contentType image) (LBS.fromStrict $ decodedImage)
