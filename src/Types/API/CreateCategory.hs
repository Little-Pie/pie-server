{-# LANGUAGE OverloadedStrings #-}

module Types.API.CreateCategory where

import Data.Aeson 

data CreateCategoryRequest = CreateCategoryRequest {name :: String
                                                   ,parentCategoryId :: Maybe Int
                                                   }

instance FromJSON CreateCategoryRequest where
  parseJSON (Object createCategoryRequest) = CreateCategoryRequest <$> createCategoryRequest .: "name"
                                                                   <*> createCategoryRequest .:? "parentCategoryId"