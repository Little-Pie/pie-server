{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.GetImageById where

import Config (Environment (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified DbQuery.Image as DB
import Endpoints.Handlers.GetImageById as Handle (GetImageByIdResult (..), Handle (..), getImageByIdHandler)
import Helpers (responseBadRequest, responseImage)
import Network.Wai (Response)
import Types.Entities.Image (Image (..))

getImageById :: Environment -> Int -> IO Response
getImageById env@Environment {..} imageId = do
  res <- getImageByIdHandler handle imageId
  case res of
    Success decodedImage contentType' -> responseImage env (BS.pack contentType') (LBS.fromStrict decodedImage)
    ImageNotExist -> responseBadRequest env "There's no images with such id"
    DecodeError -> responseBadRequest env "Couldn't decode from base64"
  where
    handle =
      Handle
        { Handle.getImageById = DB.getImageById conn
        }
