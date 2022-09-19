{-# LANGUAGE OverloadedStrings #-}

module Types.API.EditPost where

import Data.Aeson 

data EditPostRequest = EditPostRequest {title :: Maybe String
                                       ,text :: Maybe String
                                       ,categoryId :: Maybe Int
                                       }

instance FromJSON EditPostRequest where
  parseJSON (Object editPostRequest) = EditPostRequest <$> editPostRequest .:? "title"
                                                       <*> editPostRequest .:? "text"
                                                       <*> editPostRequest .:? "categoryId"