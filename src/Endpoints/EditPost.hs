{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import qualified Types.API.EditPost as API
import Types.Entities.Category
import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

editPost :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
editPost conn body postId' = case decode body :: Maybe API.EditPostRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    checkedCategoryId <-
      case API.categoryId bodyParsed of
        Just cId -> do
          categories <- query conn "select * from categories where id = (?)" [cId] :: IO [Category]
          case categories of
            [] -> pure Nothing
            _ -> pure $ API.categoryId bodyParsed
        Nothing -> pure Nothing
    posts <- query_ conn "select * from posts"
    case postId' `elem` (map postId posts) of
      False -> pure "There are no posts with such id"
      True -> do
        post' <- query conn "select * from posts where id=(?)" $ (Only postId') :: IO [Post]
        case post' of
          [] -> pure "Something went wrong: empty list"
          [x] -> do
            let newPost = x {
              title = maybe (title x) Prelude.id (API.title bodyParsed),
              text = maybe (text x) Prelude.id (API.text bodyParsed),
              categoryId = maybe (categoryId x) Prelude.id (checkedCategoryId)}
            execute conn "UPDATE posts SET (title,text,\"categoryId\") = (?,?,?) WHERE id = (?)" $ (title newPost,text newPost,categoryId newPost, postId')
            pure ("Changes applied")