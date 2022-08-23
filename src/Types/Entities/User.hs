{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entities.User where

import Data.Aeson 
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data User = User {userId :: Int
                 ,name :: String
                 ,login :: String
                 ,password :: String
                 ,createdAt :: UTCTime
                 ,isAdmin :: Bool
                 ,isAuthor :: Bool
                 }

instance ToJSON User where
  toJSON (User {..}) = object ["id" .= userId
                              ,"name" .= name
                              ,"login" .= login
                              ,"created_at" .= createdAt
                              ,"is_admin" .= isAdmin
                              ,"is_author" .= isAuthor
                              ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field