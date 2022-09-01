{-# LANGUAGE OverloadedStrings #-}

module Types.API.GetPosts where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
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
  toJSON (GetPosts postId title text categoryId createdAt authorId isPublished authorName categoryName) = object ["id" .= postId
                                                                                     ,"title" .= title
                                                                                     ,"created_at" .= createdAt
                                                                                     ,"text" .= text
                                                                                     ,"category_id" .= categoryId
                                                                                     ,"category_name" .= categoryName
                                                                                     ,"author" .= authorId
                                                                                     ,"author_name" .= authorName
                                                                                     ,"is_published" .= isPublished
                                                                                     ]

instance FromRow GetPosts where
  fromRow = GetPosts <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field