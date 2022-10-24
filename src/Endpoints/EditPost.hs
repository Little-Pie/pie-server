{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.EditPost where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, lift)
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.EditPost (EditPostResult (..), Handle (..), editPostHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditPost (EditPostRequest)
import Types.Entities.User (User)

editPost :: User -> EditPostRequest -> App Response
editPost user req = do
  Environment {..} <- ask
  res <- lift $ editPostHandler (handle conn) user req
  case res of
    Success -> responseOk "Changes applied"
    PostNotExist -> responseBadRequest "Post with such id does not exist"
    NotAuthor -> responseNotFound ""
  where
    handle conn =
      Handle
        { Endpoints.Handlers.EditPost.editPost = DBP.editPost conn,
          getCategoryById = DBC.getCategoryById conn,
          getPostById = DBP.getPostById conn
        }
