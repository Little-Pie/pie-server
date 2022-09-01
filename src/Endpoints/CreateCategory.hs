{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import Types.Entities.User
import qualified Types.API.CreateCategory as API
import Types.Entities.Category
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson

createCategory :: Connection -> LBS.ByteString -> Int -> IO LBS.ByteString
createCategory conn body userId  = case decode body :: Maybe API.CreateCategoryRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let name' = API.name bodyParsed
    let parentCategoryId' = API.parentCategoryId bodyParsed
    admin <- query conn "select * from users where id=(?)" (Only userId) :: IO [User]
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        False -> pure "You can not create categories"
        True -> do
          case parentCategoryId' of
            Nothing -> do
              categories <- query conn "select * from categories where name=(?) AND parent_id=null" (Only name') :: IO [Category]
              case categories of
                [] -> do
                  execute conn "INSERT INTO categories (name) VALUES (?)" $ (Only name')
                  pure "Category is created"
                categories' -> pure "This category already exists"
            Just parentCategoryId'' -> do
              categories <- query conn "select * from categories where name=(?) AND parent_id=(?)" $ (name',parentCategoryId') :: IO [Category]
              case categories of
                [] -> do
                  execute conn "INSERT INTO categories (name,parent_id) VALUES (?,?)" $ (name',parentCategoryId')
                  pure "Category is created"
                categories' -> pure "This category already exists"