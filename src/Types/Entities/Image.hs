{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entities.Image where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Image = Image
  { imageId :: Int,
    postId :: Int,
    base64Image :: String,
    contentType :: String
  }
  deriving (Eq, Show)

instance ToJSON Image where
  toJSON (Image {..}) =
    object
      [ "imageId" .= imageId,
        "postId" .= postId,
        "base64Image" .= base64Image,
        "contentType" .= contentType
      ]

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field <*> field
