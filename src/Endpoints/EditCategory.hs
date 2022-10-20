{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditCategory where

import Database.PostgreSQL.Simple (Connection)
import qualified DbQuery.Category as DB
import Endpoints.Handlers.EditCategory (EditCategoryResult (..), Handle (..), editCategoryHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditCategory (EditCategoryRequest)
import Types.Entities.User (User)

editCategory :: Connection -> User -> EditCategoryRequest -> IO Response
editCategory conn user req = do
  res <- editCategoryHandler handle user req
  case res of
    Success -> pure $ responseOk "Changes applied"
    NameIsTaken -> pure $ responseBadRequest "Category with such name already exists"
    CategoryNotExist -> pure $ responseBadRequest "Category with such id does not exist"
    IllegalParentCategoryId -> pure $ responseBadRequest "Illegal parent category id"
    ParentCategoryNotExist -> pure $ responseBadRequest "Parent category with such id does not exist"
    NotFound -> pure $ responseNotFound ""
  where
    handle =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
          Endpoints.Handlers.EditCategory.editCategory = DB.editCategory conn,
          getCategoryById = DB.getCategoryById conn,
          getCategoryByParentId = DB.getCategoryByParentId conn
        }
