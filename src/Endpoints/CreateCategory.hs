{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreateCategory where

import Config (Environment (..))
import qualified DbQuery.Category as DB
import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), createCategoryHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreateCategory as API
import Types.Entities.User (User)

createCategory :: Environment -> User -> API.CreateCategoryRequest -> IO Response
createCategory env@Environment {..} user req = do
  res <- createCategoryHandler handle user req
  case res of
    Success -> responseOk env "Category is created"
    NameIsTaken -> responseBadRequest env "Category with such name already exists"
    ParentCategoryNotExist -> responseBadRequest env "Parent category with such id does not exist"
    NotFound -> responseNotFound env ""
  where
    handle =
      Handle
        { getGeneralCategoryByName = DB.getGeneralCategoryByName conn,
          insertNewGeneralCategory = DB.insertNewGeneralCategory conn,
          getCategoryByNameAndParent = DB.getCategoryByNameAndParent conn,
          insertNewCategory = DB.insertNewCategory conn,
          getCategoryById = DB.getCategoryById conn
        }
