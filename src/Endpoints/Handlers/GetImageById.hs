{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.GetImageById where

import Data.ByteString.Base64 (decode)
import qualified Data.ByteString.Char8 as BS
import Types.Entities.Image (Image (..))

newtype Handle m = Handle
  { getImageById :: Int -> m [Image]
  }

data GetImageByIdResult = Success BS.ByteString String | ImageNotExist | DecodeError
  deriving (Eq, Show)

getImageByIdHandler :: (Monad m) => Handle m -> Int -> m GetImageByIdResult
getImageByIdHandler Handle {..} imageId = do
  dbImage <- getImageById imageId
  case dbImage of
    [] -> pure ImageNotExist
    (image : _) -> case decode $ BS.pack $ base64Image image of
      Left _ -> pure DecodeError
      Right decodedImage -> do
        let contentType' = contentType image
        pure $ Success decodedImage contentType'
