{-# LANGUAGE OverloadedStrings #-}

module Types.API.EditCategory where

import Data.Aeson 

data EditCategoryRequest = EditCategoryRequest {categoryId :: Int
                                               ,name :: Maybe String
                                               ,parentCategoryId :: Maybe Int
                                               }

instance FromJSON EditCategoryRequest where
  parseJSON (Object editCategoryRequest) = EditCategoryRequest <$> editCategoryRequest .: "categoryId"
                                                               <*> editCategoryRequest .:? "name"
                                                               <*> editCategoryRequest .:? "parentCategoryId"