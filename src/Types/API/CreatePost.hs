{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreatePost where

import Data.Aeson 

data CreatePostRequest = CreatePostRequest {title :: String
                                           ,text :: String
                                           ,categoryId :: Int
                                           }

instance FromJSON CreatePostRequest where
  parseJSON (Object createPostRequest) = CreatePostRequest <$> createPostRequest .: "title"
                                                           <*> createPostRequest .: "text"
                                                           <*> createPostRequest .: "categoryId"