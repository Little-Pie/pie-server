{-# LANGUAGE OverloadedStrings #-}

module Types.API.EditPost where

import Data.Aeson 

data EditPostRequest = EditPostRequest {postId :: Int
                                       ,title :: Maybe String
                                       ,text :: Maybe String
                                       ,categoryId :: Maybe Int
                                       ,isPublished :: Maybe Bool
                                       }

instance FromJSON EditPostRequest where
  parseJSON (Object editPostRequest) = EditPostRequest <$> editPostRequest .: "postId"
                                                       <*> editPostRequest .:? "title"
                                                       <*> editPostRequest .:? "text"
                                                       <*> editPostRequest .:? "categoryId"
                                                       <*> editPostRequest .:? "isPublished"