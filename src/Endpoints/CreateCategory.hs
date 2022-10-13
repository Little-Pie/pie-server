{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import qualified DbQuery.Category as DB
import Types.Entities.User (User)
import qualified Types.API.CreateCategory as API
import Database.PostgreSQL.Simple (Connection)
import Helpers (responseOk,responseBadRequest,responseNotFound)
import Network.Wai (Response)
import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), createCategoryHandler)

createCategory :: Connection -> User -> API.CreateCategoryRequest -> IO Response
createCategory conn user req = do
  res <- createCategoryHandler handle user req
  case res of
    Success -> pure $ responseOk "Category is created"
    NameIsTaken -> pure $ responseBadRequest "Category with such name already exists"
    NotFound -> pure $ responseNotFound ""
  where
    handle = Handle
      { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
        insertNewGeneralCategory = DB.insertNewGeneralCategory conn,
        getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
        insertNewCategory = DB.insertNewCategory conn
      }
