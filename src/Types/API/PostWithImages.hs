{-# LANGUAGE OverloadedStrings #-}

module Types.API.PostWithImages where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data PostWithImages = PostWithImages {postId :: Int
                 ,title :: String
                 ,text :: String
                 ,categoryId :: Int
                 ,createdAt :: UTCTime
                 ,authorId :: Int
                 ,isPublished :: Bool
                 ,authorName :: String
                 ,categoryName :: String
                 ,imagesURL :: [String]
                 }

instance ToJSON PostWithImages where
  toJSON (PostWithImages postId title text categoryId createdAt authorId isPublished authorName categoryName base64Images) = object ["id" .= postId
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
