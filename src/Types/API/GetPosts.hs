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
                 ,base64Images :: [String]
                 }

instance ToJSON GetPosts where
  toJSON (GetPosts postId title text categoryId createdAt authorId isPublished authorName categoryName base64Images) = object ["id" .= postId
                                                                                     ,"title" .= title
                                                                                     ,"createdAt" .= createdAt
                                                                                     ,"text" .= text
                                                                                     ,"categoryId" .= categoryId
                                                                                     ,"categoryName" .= categoryName
                                                                                     ,"author" .= authorId
                                                                                     ,"authorName" .= authorName
                                                                                     ,"isPublished" .= isPublished
                                                                                     ,"base64Images" .= base64Images
                                                                                     ]