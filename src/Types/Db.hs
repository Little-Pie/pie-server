{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Db where

import Database.PostgreSQL.Simple (ToRow)
import GHC.Generics (Generic)

data EditPost = EditPost
  { title :: String,
    text :: String,
    categoryId :: Int,
    postId :: Int,
    isPublished :: Bool,
    base64Images :: [String],
    contentTypes :: [String]
  }

data EditCategory = EditCategory
  { name :: String,
    parentCategoryId :: Maybe Int,
    categoryId :: Int
  }
  deriving (Generic, ToRow)
