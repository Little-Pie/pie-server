{-# LANGUAGE OverloadedStrings #-}

module Endpoints.DeletePost where

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
    let id' = API.id bodyParsed
    admin <- query conn "select * from users where id=(?)" (Only userId) :: IO [User]
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          posts <- query conn "select * from posts where id=(?)" $ (Only id') :: IO [Post]
          case posts of
            [] -> pure "There are no posts with such id"
            _ -> do
              execute conn "DELETE FROM posts WHERE id=(?)" $ (Only id')
              pure "Post is deleted"
        False -> do
          post <- query conn "select * from posts where id=(?)" (Only id') :: IO [Post]
          case post of
            [] -> pure "Something went wrong: empty list"
            [x] -> case userId == authorId x of
              False -> pure "You don't have news with such id"
              True -> do
                posts <- query conn "select * from posts where id=(?)" $ (Only id') :: IO [Post]
                case posts of
                  [] -> pure "There are no posts with such id"
                  _ -> do
                    execute conn "DELETE FROM posts WHERE id=(?)" $ (Only id')
                    pure "Post is deleted"