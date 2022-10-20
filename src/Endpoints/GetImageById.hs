{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetImageById where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple (Connection)
import qualified DbQuery.Image as DB
import Endpoints.Handlers.GetImageById as Handle (GetImageByIdResult (..), Handle (..), getImageByIdHandler)
import Helpers (responseBadRequest, responseImage)
import Network.Wai (Response)
import Types.Entities.Image (Image (..))

getImageById :: Connection -> Int -> IO Response
getImageById conn imageId = do
  res <- getImageByIdHandler handle imageId
  case res of
    Success decodedImage contentType' -> pure $ responseImage (BS.pack contentType') (LBS.fromStrict decodedImage)
    ImageNotExist -> pure $ responseBadRequest "There's no images with such id"
    DecodeError -> pure $ responseBadRequest "Couldn't decode from base64"
  where
    handle =
      Handle
        { Handle.getImageById = DB.getImageById conn
        }
