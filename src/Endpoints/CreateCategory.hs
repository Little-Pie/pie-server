module Endpoints.CreateCategory where

import Config (App)
import qualified DbQuery.Category as DB
import Endpoints.Handlers.CreateCategory
  ( CreateCategoryResult (..),
    Handle (..),
    createCategoryHandler,
  )
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreateCategory as API
import Types.Entities.User (User)

createCategory :: User -> API.CreateCategoryRequest -> App Response
createCategory user req = do
  res <- createCategoryHandler handle user req
  case res of
    Success ->
      responseOk
        "Category is created"
    NameIsTaken ->
      responseBadRequest
        "Category with such name already exists"
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
          insertNewCategory = DB.insertNewCategory,
          getCategoryById = DB.getCategoryById
        }
