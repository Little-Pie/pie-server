{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Types.Entities.User (User)
import Types.API.EditPost (EditPostRequest)
import Database.PostgreSQL.Simple (Connection)
import Helpers (responseOk, responseBadRequest, responseNotFound)
import Network.Wai (Response)
import Endpoints.Handlers.EditPost (Handle(..), EditPostResult(..), editPostHandler)

editPost :: Connection -> User -> EditPostRequest -> IO Response
editPost conn user req = do
  res <- editPostHandler handle user req
  case res of
    Success -> pure $ responseOk "Changes applied"
    PostNotExist -> pure $ responseBadRequest "Post with such id does not exist"
    NotAuthor -> pure $ responseNotFound ""
  where
    handle = Handle
      { Endpoints.Handlers.EditPost.editPost = DBP.editPost conn,
        getCategoryById = DBC.getCategoryById conn,
        getPostById = DBP.getPostById conn
      }
    