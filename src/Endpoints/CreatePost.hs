{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import Config (App)
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.CreatePost
  ( CreatePostResult (..),
    Handle (..),
    createPostHandler,
  )
import Helpers (responseBadRequest, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreatePost as API
import qualified Types.Entities.User as U

createPost :: U.User -> API.CreatePostRequest -> App Response
createPost author req = do
  res <- createPostHandler handle author req
  case res of
    Success ->
      responseOk
        "Post is created"
    CategoryNotExist ->
      responseBadRequest
        "Category with such id does not exist"
    NotAuthor ->
      responseBadRequest
        "You can not post news because you are not an author"
    WrongContentType ->
      responseBadRequest
        "Image format should be jpg, jpeg or png"
  where
    handle =
      Handle
        { getCategoryById = DBC.getCategoryById,
          insertNewPost = DBP.insertNewPost
        }
