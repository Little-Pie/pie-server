{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import DbQuery.Category
import DbQuery.Post
import DbQuery.User
import Types.Entities.Category
import Types.Entities.User
import qualified Types.API.CreatePost as API
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

createPost :: Connection -> Int -> API.CreatePostRequest -> IO Response
createPost conn authorId parsedReq = do
    author <- getUserById conn authorId
    case author of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      (x:_) -> if isAuthor x
      then do
        let base64Images' = API.base64Images parsedReq
        let contentType' = API.contentTypes parsedReq
        let title' = API.title parsedReq
        let text' = API.text parsedReq
        let categoryId' = API.categoryId parsedReq
        let isPublished' = API.isPublished parsedReq
        category <- getCategoryById conn categoryId'
        case category of
          [] -> pure $ responseBadRequest "No categories with such id"
          _ -> do
            res <- insertNewPost conn title' text' categoryId' authorId isPublished' base64Images' contentType'
            pure $ responseOk "Post is created"
      else pure $ responseNotFound "You can not post news, because you are not an author"
