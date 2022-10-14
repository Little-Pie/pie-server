{-# LANGUAGE OverloadedStrings #-}

module Types.Entities.Category where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data Category = Category {categoryId :: Int
                         ,name :: String
                         ,parentCategoryId :: Maybe Int
                         }

instance ToJSON Category where
  toJSON (Category categoryId name parentCategoryId) = object ["categoryId" .= categoryId
                                                              ,"name" .= name
                                                              ,"parentCategoryId" .= parentCategoryId
                                                              ]

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field