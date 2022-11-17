{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.GetImageById where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, lift)
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
  Environment {..} <- ask
  res <- lift $ getImageByIdHandler (handle conn) imageId
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
    handle conn =
      Handle
        { Handle.getImageById = DB.getImageById conn
        }
