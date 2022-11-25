{-# LANGUAGE DuplicateRecordFields #-}

module Types.Db where

data InsertNewUser = InsertNewUser
  { name :: String,
    login :: String,
    password :: String,
    isAdmin :: Bool,
    isAuthor :: Bool
  }

data InsertNewPost = InsertNewPost
  { title :: String,
    text :: String,
    categoryId :: Int,
    userId :: Int,
    isPublished :: Bool,
    base64Images :: [String],
    contentTypes :: [String]
  }

data EditPost = EditPost
  { title :: String,
    text :: String,
    categoryId :: Int,
    postId :: Int,
    isPublished :: Bool,
    base64Images :: [String],
    contentTypes :: [String]
  }
