{-# LANGUAGE OverloadedStrings #-}

module Types.Entities.Post where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data Post = Post {postId :: Int
                 ,title :: String
                 ,text :: String
                 ,categoryId :: Int
                 ,createdAt :: UTCTime
                 ,authorId :: Int
                 ,isPublished :: Bool
                 }

instance ToJSON Post where
  toJSON (Post postId title text categoryId createdAt authorId isPublished) = object ["id" .= postId
                                                                                     ,"title" .= title
                                                                                     ,"created_at" .= createdAt
                                                                                     ,"text" .= text
                                                                                     ,"category_id" .= categoryId
                                                                                     ,"author" .= authorId
                                                                                     ,"is_published" .= isPublished
                                                                                     ]

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field