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

createPost :: Connection -> LBS.ByteString -> Int -> IO Response
createPost conn body userId = case decode body :: Maybe API.CreatePostRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    author <- getUserById conn userId
    case author of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      (x:_) -> if isAuthor x
      then do
        let title' = API.title bodyParsed
        let text' = API.text bodyParsed
        let categoryId' = API.categoryId bodyParsed
        let isPublished' = API.isPublished bodyParsed
        category <- getCategoryById conn categoryId'
        case category of
          [] -> pure $ responseBadRequest "No categories with such id"
          _ -> do
            insertNewPost conn title' text' categoryId' userId isPublished'
            pure $ responseOk "Post is created"
      else pure $ responseNotFound "You can not post news"
