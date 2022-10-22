{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.EditPost where

import Config (Environment (..))
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.EditPost (EditPostResult (..), Handle (..), editPostHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditPost (EditPostRequest)
import Types.Entities.User (User)

editPost :: Environment -> User -> EditPostRequest -> IO Response
editPost env@Environment {..} user req = do
  res <- editPostHandler handle user req
  case res of
    Success -> responseOk env "Changes applied"
    PostNotExist -> responseBadRequest env "Post with such id does not exist"
    NotAuthor -> responseNotFound env ""
  where
    handle =
      Handle
        { Endpoints.Handlers.EditPost.editPost = DBP.editPost conn,
          getCategoryById = DBC.getCategoryById conn,
          getPostById = DBP.getPostById conn
        }
