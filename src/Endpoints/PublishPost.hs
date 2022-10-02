{-# LANGUAGE OverloadedStrings #-}

module Endpoints.PublishPost where

import qualified DbQuery.Post as DBP
import qualified DbQuery.User as DBU
import Types.Entities.User
import qualified Types.API.Id as API
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson
import Helpers
import Network.Wai (Response)

publishPost :: Connection -> LBS.ByteString -> Int -> IO Response
publishPost conn body userId = case decode body :: Maybe API.IdRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let postId = API.id bodyParsed
    admin <- DBU.getUserById conn userId
    case admin of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          posts <- DBP.getPostById conn postId
          case posts of
            [] -> pure $ responseBadRequest "There are no posts with such id"
            [x] -> if isPublished x 
                then pure $ responseBadRequest "The post is already published"
                else do 
                  DBP.publishPost conn postId
                  pure $ responseOk "Post is published"
        False -> do
          posts <- DBP.getPostById conn postId
          case posts of
            [] ->  pure $ responseBadRequest "There are no posts with such id"
            [x] -> case userId == authorId x of
              False -> pure $ responseBadRequest "You don't have unpublished news with such id"
              True -> do
                posts <- DBP.getPostById conn postId
                case posts of
                  [] -> pure $ responseBadRequest "There are no posts with such id"
                  [x] -> if isPublished x 
                    then pure $ responseBadRequest "The post is already published"
                    else do
                      DBP.publishPost conn postId
                      pure $ responseOk "Post is published"
