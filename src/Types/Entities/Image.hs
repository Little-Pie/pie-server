{-# LANGUAGE OverloadedStrings #-}

module Types.Entities.Image where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow

data Image = Image {imageId :: Int
                   ,base64Image :: String
                   ,postId :: Int
                   ,contentType :: String
                   }

instance ToJSON Image where
  toJSON (Image imageId base64Image postId contentType) = object ["imageId" .= imageId
                                                                 ,"base64Image" .= base64Image
                                                                 ,"postId" .= postId
                                                                 ,"contentType" .= contentType
                                                                 ]

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field <*> field