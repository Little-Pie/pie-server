{-# LANGUAGE OverloadedStrings #-}

module Endpoints.PublishPost where

import Types.Entities.User
import qualified Types.API.Id as API
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple
import Data.Aeson

publishPost :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
publishPost conn body userId = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let id' = API.id bodyParsed
    admin <- query conn "select * from users where id=(?)" (Only userId) :: IO [User]
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          posts <- query conn "select * from posts where id=(?)" (Only id') :: IO [Post]
          case posts of
            [] -> pure "There are no posts with such id"
            [x] -> if isPublished x 
                then pure "The post is already published"
                else do
                  execute conn "UPDATE posts SET is_published = (?) WHERE id = (?)" $ (True,id')
                  pure "Post is published"
        False -> do
          posts <- query conn "select * from posts where id=(?)" (Only id') :: IO [Post]
          case posts of
            [] ->  pure "There are no posts with such id"
            [x] -> case userId == authorId x of
              False -> pure "You don't have unpublished news with such id"
              True -> do
                posts <- query conn "select * from posts where id=(?)" (Only id') :: IO [Post]
                case posts of
                  [] -> pure "There are no posts with such id"
                  [x] -> if isPublished x 
                    then pure "The post is already published"
                    else do
                      execute conn "UPDATE posts SET is_published = (?) WHERE id = (?)" $ (True,id')
                      pure "Post is published"
