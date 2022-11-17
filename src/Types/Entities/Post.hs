{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Entities.Post where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Post = Post
  { postId :: Int,
    title :: String,
    text :: String,
    categoryId :: Int,
    createdAt :: UTCTime,
    authorId :: Int,
    isPublished :: Bool
  }
  deriving (Generic, ToJSON, FromRow)
