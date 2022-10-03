{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditCategory where

import qualified DbQuery.Category as DBC
import qualified DbQuery.User as DBU
import qualified Types.API.EditCategory as API
import Types.Entities.Category
import qualified Types.Entities.User as U
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

editCategory :: Connection -> LBS.ByteString -> Int -> IO Response
editCategory conn body authorizedUserId = case decode body :: Maybe API.EditCategoryRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let categoryId' = API.categoryId bodyParsed
    categories <- DBC.getCategoryById conn categoryId'
    case categories of
      [] -> pure $ responseBadRequest "There are no categories with such id"
      (category:_) -> do
        admin <- DBU.getUserById conn authorizedUserId
        case admin of
          [] -> pure $ responseInternalError "Something went wrong: empty list"
          (x:_) -> if U.isAdmin x
            then do
              let newCategory = category {
                name = maybe (name category) Prelude.id (API.name bodyParsed),
                parentCategoryId = maybe (parentCategoryId category) Just (API.parentCategoryId bodyParsed)}
              DBC.editCategory conn (name newCategory) (parentCategoryId newCategory) (categoryId')
              pure $ responseOk "Changes applied"
            else pure $ responseNotFound ""
