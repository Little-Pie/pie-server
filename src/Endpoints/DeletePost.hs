{-# LANGUAGE OverloadedStrings #-}

module Endpoints.DeletePost where

import DbQuery.User
import qualified DbQuery.Post as DB
import Types.Entities.User
import qualified Types.API.Id as API
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Helpers
import Network.Wai (Response)

deletePost :: Connection -> LBS.ByteString -> Int -> IO Response
deletePost conn body userId = case decode body :: Maybe API.IdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let postId = API.id bodyParsed
    admin <- getUserById conn userId
    case admin of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          posts <- DB.getPostById conn postId
          case posts of
            [] -> pure $ responseBadRequest "There are no posts with such id"
            _ -> do
              DB.deletePost conn postId
              pure $ responseOk "Post is deleted"
        False -> do
          post <- DB.getPostById conn postId
          case post of
            [] -> pure $ responseInternalError "Something went wrong: empty list"
            [x] -> case userId == authorId x of
              False -> pure $ responseBadRequest "You don't have news with such id"
              True -> do
                posts <- DB.getPostById conn postId
                case posts of
                  [] -> pure $ responseBadRequest "There are no posts with such id"
                  _ -> do 
                    DB.deletePost conn postId
                    pure $ responseOk "Post is deleted"
