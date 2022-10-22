{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.EditCategory where

import Config (Environment (..))
import qualified DbQuery.Category as DB
import Endpoints.Handlers.EditCategory (EditCategoryResult (..), Handle (..), editCategoryHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditCategory (EditCategoryRequest)
import Types.Entities.User (User)

editCategory :: Environment -> User -> EditCategoryRequest -> IO Response
editCategory env@Environment {..} user req = do
  res <- editCategoryHandler handle user req
  case res of
    Success -> responseOk env "Changes applied"
    NameIsTaken -> responseBadRequest env "Category with such name already exists"
    CategoryNotExist -> responseBadRequest env "Category with such id does not exist"
    IllegalParentCategoryId -> responseBadRequest env "Illegal parent category id"
    ParentCategoryNotExist -> responseBadRequest env "Parent category with such id does not exist"
    NotFound -> responseNotFound env ""
  where
    handle =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
          Endpoints.Handlers.EditCategory.editCategory = DB.editCategory conn,
          getCategoryById = DB.getCategoryById conn,
          getCategoryByParentId = DB.getCategoryByParentId conn
        }
