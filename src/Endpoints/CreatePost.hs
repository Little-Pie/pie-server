{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import DbQuery.Category
import DbQuery.Post
import Types.Entities.Category
import qualified Types.API.CreatePost as CreatePost
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

createPost :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
createPost conn body userId = case decode body :: Maybe CreatePost.CreatePostRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let title' = CreatePost.title bodyParsed
    let text' = CreatePost.text bodyParsed
    let categoryId' = CreatePost.categoryId bodyParsed
    category <- getCategoryById conn categoryId'
    case category of
      [] -> pure "No categories with such id"
      _ -> insertNewPost conn title' text' categoryId' userId