{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import qualified Types.API.CreatePost as CreatePost
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Network.HTTP.Types (Query)
import Database.PostgreSQL.Simple

createPost :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
createPost conn body = case decode body :: Maybe CreatePost.CreatePostRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let title' = CreatePost.title bodyParsed
    let text' = CreatePost.text bodyParsed
    execute conn "INSERT INTO posts (title,text,author_id,is_published) VALUES (?,?,?,?)" $ (title',text',1 :: Int,False)
    pure "Post is created"