{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import qualified Types.API.EditPost as API
import Types.Entities.Category
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

editPost :: Connection -> LBS.ByteString -> Int -> IO Response
editPost conn body postId' = case decode body :: Maybe API.EditPostRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    checkedCategoryId <-
      case API.categoryId bodyParsed of
        Just cId -> do
          categories <- DBC.getCategoryById conn cId
          case categories of
            [] -> pure Nothing
            _ -> pure $ API.categoryId bodyParsed
        Nothing -> pure Nothing
    posts <- DBP.getPostById conn postId'
    case posts of
      [] -> pure $ responseBadRequest "There are no posts with such id"
      (x:_) -> do
        let newPost = x {
          title = maybe (title x) Prelude.id (API.title bodyParsed),
          text = maybe (text x) Prelude.id (API.text bodyParsed),
          categoryId = maybe (categoryId x) Prelude.id (checkedCategoryId)}
        DBP.editPost conn (title newPost) (text newPost) (categoryId newPost) (postId')
        pure $ responseOk "Changes applied"