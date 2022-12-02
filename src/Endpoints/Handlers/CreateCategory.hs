{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreateCategory where

import Types.API.CreateCategory (CreateCategoryRequest (..))
import qualified Types.Entities.Category as C
import qualified Types.Entities.User as U

data Handle m = Handle
  { getGeneralCategoryByName :: String -> m [C.Category],
    getCategoryByNameAndParent :: String -> Int -> m [C.Category],
    insertNewCategory :: CreateCategoryRequest -> m (),
    getCategoryById :: Int -> m [C.Category]
  }

data CreateCategoryResult
  = Success
  | NameIsTaken
  | ParentCategoryNotExist
  | NotFound
  deriving (Eq, Show)

createCategoryHandler ::
  (Monad m) =>
  Handle m ->
  U.User ->
  CreateCategoryRequest ->
  m CreateCategoryResult
createCategoryHandler Handle {..} user req@CreateCategoryRequest {..} =
  if U.isAdmin user
    then case parentCategoryId of
      Nothing -> do
        categories <- getGeneralCategoryByName name
        case categories of
          [] -> do
            insertNewCategory req
            pure Success
          _ -> pure NameIsTaken
      Just parentCategoryId' -> do
        parentCategories <- getCategoryById parentCategoryId'
        case parentCategories of
          [] -> pure ParentCategoryNotExist
          _ -> do
            categories <- getCategoryByNameAndParent name parentCategoryId'
            case categories of
              [] -> do
                insertNewCategory req
                pure Success
              _ -> pure NameIsTaken
    else pure NotFound
