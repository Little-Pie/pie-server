{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreateCategory where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, lift)
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
  Environment {..} <- ask
  res <- lift $ createCategoryHandler (handle conn) user req
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
    handle conn =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
          insertNewGeneralCategory = DB.insertNewGeneralCategory conn,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
          insertNewCategory = DB.insertNewCategory conn,
          getCategoryById = DB.getCategoryById conn
        }
