{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import qualified DbQuery.User as DBU
import qualified Types.API.EditPost as API
import Types.Entities.Category
import Types.Entities.Post as Post
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)
import Data.Maybe (fromMaybe)

editPost :: Connection -> User -> API.EditPostRequest -> IO Response
editPost conn user parsedReq = do
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
        if userId user == authorId post
          then do
            let newPost = post {
              title = fromMaybe (title post) (API.title parsedReq),
              text = fromMaybe (text post) (API.text parsedReq),
              Post.categoryId = fromMaybe (Post.categoryId post) checkedCategoryId,
              isPublished = fromMaybe (isPublished post) (API.isPublished parsedReq)}
            let base64Images = fromMaybe [] (API.base64Images parsedReq)
            let contentTypes = fromMaybe [] (API.contentTypes parsedReq)
            DBP.editPost conn (title newPost) (text newPost) (Post.categoryId newPost) editPostId (isPublished newPost) base64Images contentTypes
            pure $ responseOk "Changes applied"
          else pure $ responseNotFound ""