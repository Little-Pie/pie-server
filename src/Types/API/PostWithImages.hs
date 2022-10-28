{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.API.PostWithImages where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Time.Clock (UTCTime)

data PostWithImages = PostWithImages
  { postId :: Int,
    title :: String,
    text :: String,
    categoryId :: Int,
    createdAt :: UTCTime,
    authorId :: Int,
    isPublished :: Bool,
    authorName :: String,
    categoryName :: String,
    imagesURL :: [String]
  }

instance ToJSON PostWithImages where
  toJSON (PostWithImages {..}) =
    object
      [ "id" .= postId,
        "title" .= title,
        "createdAt" .= createdAt,
        "text" .= text,
        "categoryId" .= categoryId,
        "categoryName" .= categoryName,
        "author" .= authorId,
        "authorName" .= authorName,
        "isPublished" .= isPublished,
        "imagesURL" .= imagesURL
      ]
