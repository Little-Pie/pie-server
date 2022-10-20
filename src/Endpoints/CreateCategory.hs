{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import Database.PostgreSQL.Simple (Connection)
import qualified DbQuery.Category as DB
import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), createCategoryHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreateCategory as API
import Types.Entities.User (User)

createCategory :: Connection -> User -> API.CreateCategoryRequest -> IO Response
createCategory conn user req = do
  res <- createCategoryHandler handle user req
  case res of
    Success -> pure $ responseOk "Category is created"
    NameIsTaken -> pure $ responseBadRequest "Category with such name already exists"
    ParentCategoryNotExist -> pure $ responseBadRequest "Parent category with such id does not exist"
    NotFound -> pure $ responseNotFound ""
  where
    handle =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
          insertNewGeneralCategory = DB.insertNewGeneralCategory conn,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
          insertNewCategory = DB.insertNewCategory conn,
          getCategoryById = DB.getCategoryById conn
        }
