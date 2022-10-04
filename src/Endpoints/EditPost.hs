{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import qualified DbQuery.User as DBU
import qualified Types.API.EditPost as API
import Types.Entities.Category
import Types.Entities.Post
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

editPost :: Connection -> LBS.ByteString -> Int -> IO Response
editPost conn body authorizedUserId = case decode body :: Maybe API.EditPostRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let editPostId = API.postId bodyParsed
    checkedCategoryId <-
      case API.categoryId bodyParsed of
        Just cId -> do
          categories <- DBC.getCategoryById conn cId
          case categories of
            [] -> pure Nothing
            _ -> pure $ API.categoryId bodyParsed
        Nothing -> pure Nothing
    posts <- DBP.getPostById conn editPostId
    case posts of
      [] -> pure $ responseBadRequest "There are no posts with such id"
      (post:_) -> do
        if authorizedUserId == authorId post
          then do
            let newPost = post {
              title = maybe (title post) Prelude.id (API.title bodyParsed),
              text = maybe (text post) Prelude.id (API.text bodyParsed),
              categoryId = maybe (categoryId post) Prelude.id (checkedCategoryId),
              isPublished = maybe (isPublished post) Prelude.id (API.isPublished bodyParsed)}
            let base64Images = maybe [] Prelude.id (API.base64Images bodyParsed)
            let contentTypes = maybe [] Prelude.id (API.contentTypes bodyParsed)
            DBP.editPost conn (title newPost) (text newPost) (categoryId newPost) (editPostId) (isPublished newPost) base64Images contentTypes
            pure $ responseOk "Changes applied"
          else pure $ responseNotFound ""