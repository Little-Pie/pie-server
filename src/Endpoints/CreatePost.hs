{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import qualified Types.Entities.User as U
import qualified Types.API.CreatePost as API
import Database.PostgreSQL.Simple (Connection)
import Helpers (responseOk, responseBadRequest)
import Network.Wai (Response)
import Endpoints.Handlers.CreatePost (CreatePostResult (..), Handle (..), createPostHandler)

createPost :: Connection -> U.User -> API.CreatePostRequest -> IO Response
createPost conn author req = do
  res <- createPostHandler handle author req
  case res of
    Success -> pure $ responseOk "Post is created"
    CategoryNotExist -> pure $ responseBadRequest "Category with such id does not exist"
    NotAuthor -> pure $ responseBadRequest "You can not post news because you are not an author"
  where
    handle = Handle
      {getCategoryById = DBC.getCategoryById conn,
       insertNewPost = DBP.insertNewPost conn
      }
