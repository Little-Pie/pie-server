{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

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
    let category_id' = CreatePost.categoryId bodyParsed
    category <- query conn "select * from categories where id=(?)" (Only category_id') :: IO [Category]
    case category of
      [] -> pure "No categories with such id"
      _ -> do
        execute conn "INSERT INTO posts (title,text,author_id,is_published,category_id) VALUES (?,?,?,?,?)" $ (title',text',userId,False,category_id')
        pure "Post is created"