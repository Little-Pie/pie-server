{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.EditCategory where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, lift)
import qualified DbQuery.Category as DB
import Endpoints.Handlers.EditCategory (EditCategoryResult (..), Handle (..), editCategoryHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditCategory (EditCategoryRequest)
import Types.Entities.User (User)

editCategory :: User -> EditCategoryRequest -> App Response
editCategory user req = do
  Environment {..} <- ask
  res <- lift $ editCategoryHandler (handle conn) user req
  case res of
    Success -> responseOk "Changes applied"
    NameIsTaken -> responseBadRequest "Category with such name already exists"
    CategoryNotExist -> responseBadRequest "Category with such id does not exist"
    IllegalParentCategoryId -> responseBadRequest "Illegal parent category id"
    ParentCategoryNotExist -> responseBadRequest "Parent category with such id does not exist"
    NotFound -> responseNotFound ""
  where
    handle conn =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
          Endpoints.Handlers.EditCategory.editCategory = DB.editCategory conn,
          getCategoryById = DB.getCategoryById conn,
          getCategoryByParentId = DB.getCategoryByParentId conn
        }
