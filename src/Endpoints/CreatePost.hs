{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreatePost where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, lift)
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.CreatePost (CreatePostResult (..), Handle (..), createPostHandler)
import Helpers (responseBadRequest, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreatePost as API
import qualified Types.Entities.User as U

createPost :: U.User -> API.CreatePostRequest -> App Response
createPost author req = do
  Environment {..} <- ask
  res <- lift $ createPostHandler (handle conn) author req
  case res of
    Success -> responseOk "Post is created"
    CategoryNotExist -> responseBadRequest "Category with such id does not exist"
    NotAuthor -> responseBadRequest "You can not post news because you are not an author"
  where
    handle conn =
      Handle
        { getCategoryById = DBC.getCategoryById conn,
          insertNewPost = DBP.insertNewPost conn
        }
