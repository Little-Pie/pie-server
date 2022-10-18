{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entities.GetPosts where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Data.Time.Clock (UTCTime)

data GetPosts = GetPosts {postId :: Int
                         ,title :: String
                         ,text :: String
                         ,categoryId :: Int
                         ,createdAt :: UTCTime
                         ,authorId :: Int
                         ,isPublished :: Bool
                         ,authorName :: String
                         ,categoryName :: String
                         }

instance ToJSON GetPosts where
  toJSON (GetPosts {..}) = 
    object ["id" .= postId
           ,"title" .= title
           ,"createdAt" .= createdAt
           ,"text" .= text
           ,"categoryId" .= categoryId
           ,"categoryName" .= categoryName
           ,"author" .= authorId
           ,"authorName" .= authorName
           ,"isPublished" .= isPublished
           ]

instance FromRow GetPosts where
  fromRow = GetPosts <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
