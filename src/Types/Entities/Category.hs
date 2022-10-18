{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entities.Category where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Data.Time.Clock (UTCTime)

data Category = Category {categoryId :: Int
                         ,name :: String
                         ,parentCategoryId :: Maybe Int
                         }

instance ToJSON Category where
  toJSON (Category {..}) = 
    object 
      [ "categoryId" .= categoryId,
        "name" .= name,
        "parentCategoryId" .= parentCategoryId
      ]

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field
