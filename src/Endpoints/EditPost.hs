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

editPost :: Connection -> Int -> API.EditPostRequest -> IO Response
editPost conn authorizedUserId parsedReq = do
    let editPostId = API.postId parsedReq
    checkedCategoryId <-
      case API.categoryId parsedReq of
        Just cId -> do
          categories <- DBC.getCategoryById conn cId
          case categories of
            [] -> pure Nothing
            _ -> pure $ API.categoryId parsedReq
        Nothing -> pure Nothing
    posts <- DBP.getPostById conn editPostId
    case posts of
      [] -> pure $ responseBadRequest "There are no posts with such id"
      (post:_) -> do
        if authorizedUserId == authorId post
          then do
            let newPost = post {
              title = maybe (title post) Prelude.id (API.title parsedReq),
              text = maybe (text post) Prelude.id (API.text parsedReq),
              categoryId = maybe (categoryId post) Prelude.id (checkedCategoryId),
              isPublished = maybe (isPublished post) Prelude.id (API.isPublished parsedReq)}
            let base64Images = maybe [] Prelude.id (API.base64Images parsedReq)
            let contentTypes = maybe [] Prelude.id (API.contentTypes parsedReq)
            DBP.editPost conn (title newPost) (text newPost) (categoryId newPost) (editPostId) (isPublished newPost) base64Images contentTypes
            pure $ responseOk "Changes applied"
          else pure $ responseNotFound ""