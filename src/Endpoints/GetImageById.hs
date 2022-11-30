{-# LANGUAGE OverloadedStrings #-}

module Endpoints.GetImageById where

import Config (App)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified DbQuery.Image as DB
import Endpoints.Handlers.GetImageById as Handle
  ( GetImageByIdResult (..),
    Handle (..),
    getImageByIdHandler,
  )
import Helpers (responseBadRequest, responseImage)
import Network.Wai (Response)

getImageById :: Int -> App Response
getImageById imageId = do
  res <- getImageByIdHandler handle imageId
  case res of
    Success decodedImage contentType' ->
      responseImage
        (BS.pack contentType')
        (LBS.fromStrict decodedImage)
    ImageNotExist ->
      responseBadRequest
        "There's no images with such id"
    DecodeError ->
      responseBadRequest
        "Couldn't decode from base64"
  where
    handle =
      Handle
        { Handle.getImageById = DB.getImageById
        }
