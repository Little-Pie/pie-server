{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entities.User where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data User = User
  { userId :: Int,
    name :: String,
    login :: String,
    password :: String,
    createdAt :: UTCTime,
    isAdmin :: Bool,
    isAuthor :: Bool
  }

instance ToJSON User where
  toJSON (User {..}) =
    object
      [ "id" .= userId,
        "name" .= name,
        "login" .= login,
        "createdAt" .= createdAt,
        "isAdmin" .= isAdmin,
        "isAuthor" .= isAuthor
      ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field
