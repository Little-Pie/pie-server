{-# LANGUAGE OverloadedStrings #-}

module Types.Entities.Category where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data Category = Category {id :: Int
                         ,name :: String
                         ,parent_category_id :: Int
                         }

instance ToJSON Category where
  toJSON (Category id name parent_category_id) = object ["id" .= id
                                                        ,"name" .= name
                                                        ,"parent_category_id" .= parent_category_id
                                                        ]

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field