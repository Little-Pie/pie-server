module Types.Entities.GetPosts where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data GetPosts = GetPosts
  { postId :: Int,
    title :: String,
    text :: String,
    categoryId :: Int,
    createdAt :: UTCTime,
    authorId :: Int,
    isPublished :: Bool,
    authorName :: String,
    categoryName :: String
  }
  deriving (Generic, ToJSON, FromRow)
