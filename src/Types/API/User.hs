{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.API.User where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data User = User
  { userId :: Int,
    name :: String,
    login :: String,
    createdAt :: UTCTime,
    isAdmin :: Bool,
    isAuthor :: Bool
  }
  deriving (Generic, ToJSON, FromRow)
