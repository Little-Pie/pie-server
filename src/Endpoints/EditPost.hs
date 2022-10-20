{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import Database.PostgreSQL.Simple (Connection)
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.EditPost (EditPostResult (..), Handle (..), editPostHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditPost (EditPostRequest)
import Types.Entities.User (User)

editPost :: Connection -> User -> EditPostRequest -> IO Response
editPost conn user req = do
  res <- editPostHandler handle user req
  case res of
    Success -> pure $ responseOk "Changes applied"
    PostNotExist -> pure $ responseBadRequest "Post with such id does not exist"
    NotAuthor -> pure $ responseNotFound ""
  where
    handle =
      Handle
        { Endpoints.Handlers.EditPost.editPost = DBP.editPost conn,
          getCategoryById = DBC.getCategoryById conn,
          getPostById = DBP.getPostById conn
        }
