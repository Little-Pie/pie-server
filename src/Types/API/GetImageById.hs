{-# LANGUAGE OverloadedStrings #-}

module Types.API.GetImageById where

import Data.Aeson 

newtype GetImageByIdRequest = GetImageByIdRequest {imageId :: Int}

instance FromJSON GetImageByIdRequest where
  parseJSON (Object getImageByIdRequest) = GetImageByIdRequest <$> getImageByIdRequest .: "imageId"