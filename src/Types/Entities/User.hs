module Types.Entities.User where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data User = User
  { userId :: Int,
    name :: String,
    login :: String,
    password :: String,
    createdAt :: UTCTime,
    isAdmin :: Bool,
    isAuthor :: Bool
  }
  deriving (Generic, ToJSON, FromRow)
