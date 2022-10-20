{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entities.Post where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Post = Post
  { postId :: Int,
    title :: String,
    text :: String,
    categoryId :: Int,
    createdAt :: UTCTime,
    authorId :: Int,
    isPublished :: Bool
  }

instance ToJSON Post where
  toJSON (Post {..}) =
    object
      [ "id" .= postId,
        "title" .= title,
        "createdAt" .= createdAt,
        "text" .= text,
        "categoryId" .= categoryId,
        "author" .= authorId,
        "isPublished" .= isPublished
      ]

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field
