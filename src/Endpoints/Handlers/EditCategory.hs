{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.EditCategory where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Types.API.EditCategory (EditCategoryRequest (..))
import qualified Types.Entities.Category as C
import qualified Types.Entities.User as U

data Handle m = Handle
  { getGeneralCategoryByName :: String -> m [C.Category],
    getCategoryByNameAndParent :: String -> Int -> m [C.Category],
    editCategory :: String -> Maybe Int -> Int -> m (),
    getCategoryById :: Int -> m [C.Category],
    getCategoryByParentId :: [Int] -> m [C.Category]
  }

data EditCategoryResult
  = Success
  | NameIsTaken
  | CategoryNotExist
  | IllegalParentCategoryId
  | ParentCategoryNotExist
  | NotFound
  deriving (Eq, Show)

editCategoryHandler ::
  (Monad m) =>
  Handle m ->
  U.User ->
  EditCategoryRequest ->
  m EditCategoryResult
editCategoryHandler Handle {..} user EditCategoryRequest {..} = do
  categories <- getCategoryById categoryId
  case categories of
    [] -> pure CategoryNotExist
    (category : _) -> do
      let newCategory =
            category
              { C.name = fromMaybe (C.name category) name,
                C.parentCategoryId = parentCategoryId <|> C.parentCategoryId category
              }
      if U.isAdmin user
        then case C.parentCategoryId newCategory of
          Nothing -> do
            checkParentCategories <- getGeneralCategoryByName (C.name newCategory)
            case checkParentCategories of
              [] -> do
                editCategory
                  (C.name newCategory)
                  (C.parentCategoryId category)
                  categoryId
                pure Success
              _ -> pure NameIsTaken
          Just parentCategoryId' ->
            if categoryId == parentCategoryId'
              then pure IllegalParentCategoryId
              else do
                childCategoryIds <- checkChildCategories [categoryId] []
                if parentCategoryId' `elem` childCategoryIds
                  then pure IllegalParentCategoryId
                  else do
                    parentCategories <- getCategoryById parentCategoryId'
                    case parentCategories of
                      [] -> pure ParentCategoryNotExist
                      _ -> do
                        checkCategories <-
                          getCategoryByNameAndParent
                            (C.name newCategory)
                            parentCategoryId'
                        case checkCategories of
                          [] -> do
                            editCategory
                              (C.name newCategory)
                              (C.parentCategoryId newCategory)
                              categoryId
                            pure Success
                          _ -> pure NameIsTaken
        else pure NotFound
  where
    checkChildCategories [] acc = pure acc
    checkChildCategories ids acc = do
      categories <- getCategoryByParentId ids
      case map C.categoryId categories of
        [] -> checkChildCategories [] acc
        childIds -> checkChildCategories childIds (acc ++ childIds)
