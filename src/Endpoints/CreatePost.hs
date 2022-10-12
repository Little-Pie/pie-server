{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreatePost where

import DbQuery.Category
import DbQuery.Post
import DbQuery.User
import Types.Entities.Category
import qualified Types.Entities.User as U
import Types.API.CreatePost
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

createPost :: Connection -> U.User -> CreatePostRequest -> IO Response
createPost conn author CreatePostRequest {..} = do
  if U.isAuthor author
    then do
      category <- getCategoryById conn categoryId
      case category of
        [] -> pure $ responseBadRequest "No categories with such id"
        _ -> do
          insertNewPost conn title text categoryId (U.userId author) isPublished base64Images contentTypes
          pure $ responseOk "Post is created"
    else pure $ responseNotFound "You can not post news, because you are not an author"
