{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import DbQuery.User
import DbQuery.Category
import Types.Entities.User
import Types.Entities.Category
import qualified Types.API.CreateCategory as API
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson

createCategory :: Connection -> LBS.ByteString -> Int -> IO LBS.ByteString
createCategory conn body userId  = case decode body :: Maybe API.CreateCategoryRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let name' = API.name bodyParsed
    let mbParentCategoryId = API.parentCategoryId bodyParsed
    admin <- getUserById conn userId
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        False -> pure "You can not create categories"
        True -> do
          case mbParentCategoryId of
            Nothing -> do
              categories <- getGeneralCategoryByName conn name'
              case categories of
                [] -> do
                  insertNewGeneralCategory conn name'
                  pure "Category is created"
                categories' -> pure "This category already exists"
            Just parentCategoryId' -> do
              categories <- getCategoryByNameAndParent conn name' parentCategoryId'
              case categories of
                [] -> do
                  insertNewCategory conn name' parentCategoryId'
                  pure "Category is created"
                categories' -> pure "This category already exists"