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
import Helpers
import Network.Wai (Response)

createCategory :: Connection -> Int -> API.CreateCategoryRequest -> IO Response
createCategory conn userId parsedReq = do
    let name' = API.name parsedReq
    let mbParentCategoryId = API.parentCategoryId parsedReq
    admin <- getUserById conn userId
    case admin of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      (x:_) -> if isAdmin x
        then do
          case mbParentCategoryId of
            Nothing -> do
              categories <- getGeneralCategoryByName conn name'
              case categories of
                [] -> do
                  insertNewGeneralCategory conn name'
                  pure $ responseOk "Category is created"
                categories' -> pure $ responseBadRequest "This category already exists"
            Just parentCategoryId' -> do
              categories <- getCategoryByNameAndParent conn name' parentCategoryId'
              case categories of
                [] -> do
                  insertNewCategory conn name' parentCategoryId'
                  pure $ responseOk "Category is created"
                categories' -> pure $ responseBadRequest "This category already exists"
        else pure $ responseNotFound ""
