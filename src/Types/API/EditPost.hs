{-# LANGUAGE OverloadedStrings #-}

module Types.API.EditPost where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:), (.:?))

data EditPostRequest = EditPostRequest
  { postId :: Int,
    title :: Maybe String,
    text :: Maybe String,
    categoryId :: Maybe Int,
    isPublished :: Maybe Bool,
    base64Images :: Maybe [String],
    contentTypes :: Maybe [String]
  }

instance FromJSON EditPostRequest where
  parseJSON (Object editPostRequest) =
    EditPostRequest <$> editPostRequest .: "postId"
      <*> editPostRequest .:? "title"
      <*> editPostRequest .:? "text"
      <*> editPostRequest .:? "categoryId"
      <*> editPostRequest .:? "isPublished"
      <*> editPostRequest .:? "base64Images"
      <*> editPostRequest .:? "contentTypes"
  parseJSON _ = mzero
