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

editCategory :: Connection -> Int -> API.EditCategoryRequest -> IO Response
editCategory conn authorizedUserId parsedReq = do
    let categoryId' = API.categoryId parsedReq
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
                name = maybe (name category) Prelude.id (API.name parsedReq),
                parentCategoryId = maybe (parentCategoryId category) Just (API.parentCategoryId parsedReq)}
              DBC.editCategory conn (name newCategory) (parentCategoryId newCategory) (categoryId')
              pure $ responseOk "Changes applied"
            else pure $ responseNotFound ""
