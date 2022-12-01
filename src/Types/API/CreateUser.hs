{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.API.CreateUser where

import Data.Aeson (FromJSON)
import Database.PostgreSQL.Simple (ToRow)
import GHC.Generics (Generic)

data CreateUserRequest = CreateUserRequest
  { name :: String,
    login :: String,
    password :: String,
    isAdmin :: Bool,
    isAuthor :: Bool
  }
  deriving (Generic, FromJSON, ToRow)
