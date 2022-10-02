{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditCategory where

import qualified DbQuery.Category as DB
import qualified Types.API.EditCategory as API
import Types.Entities.Category
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

editCategory :: Connection -> LBS.ByteString -> IO Response
editCategory conn body = case decode body :: Maybe API.EditCategoryRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let categoryId' = API.categoryId bodyParsed
    categories <- DB.getCategoryById conn categoryId'
    case categories of
      [] -> pure $ responseBadRequest "There are no categories with such id"
      (x:_) -> do
        let newCategory = x {
          name = maybe (name x) Prelude.id (API.name bodyParsed),
          parentCategoryId = maybe (parentCategoryId x) Just (API.parentCategoryId bodyParsed)}
        DB.editCategory conn (name newCategory) (parentCategoryId newCategory) (categoryId')
        pure $ responseOk "Changes applied"