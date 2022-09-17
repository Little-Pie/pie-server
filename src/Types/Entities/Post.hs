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
                                                                                     ,"createdAt" .= createdAt
                                                                                     ,"text" .= text
                                                                                     ,"categoryId" .= categoryId
                                                                                     ,"author" .= authorId
                                                                                     ,"isPublished" .= isPublished
                                                                                     ]

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field