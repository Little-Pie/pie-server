{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreatePost where

import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))

data CreatePostRequest = CreatePostRequest
  { title :: String,
    text :: String,
    categoryId :: Int,
    isPublished :: Bool,
    base64Images :: [String],
    contentTypes :: [String]
  }

instance FromJSON CreatePostRequest where
  parseJSON (Object createPostRequest) =
    CreatePostRequest <$> createPostRequest .: "title"
      <*> createPostRequest .: "text"
      <*> createPostRequest .: "categoryId"
      <*> createPostRequest .: "isPublished"
      <*> createPostRequest .: "base64Images"
      <*> createPostRequest .: "contentTypes"
