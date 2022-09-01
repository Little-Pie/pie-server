{-# LANGUAGE OverloadedStrings #-}

module Types.Entities.Category where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data Category = Category {id :: Int
                         ,name :: String
                         ,parentCategoryId :: Maybe Int
                         }

instance ToJSON Category where
  toJSON (Category id name parentCategoryId) = object ["id" .= id
                                                      ,"name" .= name
                                                      ,"parent_category_id" .= parentCategoryId
                                                      ]

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field