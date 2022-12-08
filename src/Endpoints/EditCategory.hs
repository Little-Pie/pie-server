module Endpoints.EditCategory where

import Config (App)
import qualified DbQuery.Category as DB
import Endpoints.Handlers.EditCategory
  ( EditCategoryResult (..),
    Handle (..),
    editCategoryHandler,
  )
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditCategory (EditCategoryRequest)
import Types.Entities.User (User)

editCategory :: User -> EditCategoryRequest -> App Response
editCategory user req = do
  res <- editCategoryHandler handle user req
  case res of
    Success ->
      responseOk
        "Changes applied"
    NameIsTaken ->
      responseBadRequest
        "Category with such name already exists"
    CategoryNotExist ->
      responseBadRequest
        "Category with such id does not exist"
    IllegalParentCategoryId ->
      responseBadRequest
        "Illegal parent category id"
    ParentCategoryNotExist ->
      responseBadRequest
        "Parent category with such id does not exist"
    NotFound ->
      responseNotFound
        ""
  where
    handle =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent,
          Endpoints.Handlers.EditCategory.editCategory = DB.editCategory,
          getCategoryById = DB.getCategoryById,
          getCategoryByParentId = DB.getCategoryByParentId
        }
