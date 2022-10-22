{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreatePost where

import Config (Environment (..))
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.CreatePost (CreatePostResult (..), Handle (..), createPostHandler)
import Helpers (responseBadRequest, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreatePost as API
import qualified Types.Entities.User as U

createPost :: Environment -> U.User -> API.CreatePostRequest -> IO Response
createPost env@Environment {..} author req = do
  res <- createPostHandler handle author req
  case res of
    Success -> responseOk env "Post is created"
    CategoryNotExist -> responseBadRequest env "Category with such id does not exist"
    NotAuthor -> responseBadRequest env "You can not post news because you are not an author"
  where
    handle =
      Handle
        { getCategoryById = DBC.getCategoryById conn,
          insertNewPost = DBP.insertNewPost conn
        }
