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

deletePost :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
deletePost conn body userId = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let postId = API.id bodyParsed
    admin <- getUserById conn userId
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          posts <- DB.getPostById conn postId
          case posts of
            [] -> pure "There are no posts with such id"
            _ -> DB.deletePost conn postId
        False -> do
          post <- DB.getPostById conn postId
          case post of
            [] -> pure "Something went wrong: empty list"
            [x] -> case userId == authorId x of
              False -> pure "You don't have news with such id"
              True -> do
                posts <- DB.getPostById conn postId
                case posts of
                  [] -> pure "There are no posts with such id"
                  _ -> DB.deletePost conn postId