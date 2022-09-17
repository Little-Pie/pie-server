{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import DBReq.DBReqCategory
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
    admin <- query conn "select * from users where id=(?)" (Only userId) :: IO [User]
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        False -> pure "You can not create categories"
        True -> do
          case mbParentCategoryId of
            Nothing -> do
              categories <- getGeneralCategoryWithName conn name'
              case categories of
                [] -> do
                  insertNewGeneralCategory conn name'
                  pure "Category is created"
                categories' -> pure "This category already exists"
            Just parentCategoryId' -> do
              categories <- getCategoryWithNameAndParent conn name' parentCategoryId'
              case categories of
                [] -> do
                  insertNewCategory conn name' parentCategoryId'
                  pure "Category is created"
                categories' -> pure "This category already exists"