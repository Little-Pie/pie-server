{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import qualified Types.API.EditPost as API
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

editPost :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
editPost conn body postId' = case decode body :: Maybe API.EditPostRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    posts <- query_ conn "select * from posts"
    case postId' `elem` (map postId posts) of
      False -> pure "There are no posts with such id"
      True -> do
        post' <- query conn "select * from posts where id=(?)" $ (Only postId') :: IO [Post]
        case post' of
          [] -> pure "Something went wrong: empty list"
          [x] -> do
            let newPost = x {
              title = maybe (title x) id (API.title bodyParsed),
              text = maybe (text x) id (API.text bodyParsed)}
            execute conn "UPDATE posts SET (title,text) = (?,?) WHERE id = (?)" $ (title newPost,text newPost,postId')
            pure ("Changes applied")