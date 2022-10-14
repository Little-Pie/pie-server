{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreateCategory where

import qualified Types.Entities.User as U
import qualified Types.Entities.Category as C
import Types.API.CreateCategory (CreateCategoryRequest (..))

data Handle m = Handle
  { getGeneralCategoryByName :: String -> m [C.Category],
    insertNewGeneralCategory :: String -> m (),
    getCategoryByNameAndParent :: String -> Int -> m [C.Category],
    insertNewCategory :: String -> Int -> m (),
    getCategoryById :: Int -> m [C.Category]
  }

data CreateCategoryResult = Success | NameIsTaken | ParentCategoryNotExist | NotFound
  deriving (Eq, Show)

createCategoryHandler :: (Monad m) => Handle m -> U.User -> CreateCategoryRequest -> m CreateCategoryResult
createCategoryHandler Handle {..} user CreateCategoryRequest {..} =
  if U.isAdmin user
    then case parentCategoryId of
      Nothing -> do
        categories <- getGeneralCategoryByName name
        case categories of
          [] -> do
            insertNewGeneralCategory name
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
                insertNewCategory name parentCategoryId'
                pure Success
              _ -> pure NameIsTaken
    else pure NotFound

