{-# LANGUAGE OverloadedStrings #-}

module Endpoints.DeletePost where

import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

deletePost :: Connection -> Int -> IO (LBS.ByteString)
deletePost conn postId' = do
    posts <- query_ conn "select * from posts"
    case postId' `elem` (map postId posts) of
        False -> pure "There are no posts with such id"
        True -> do
          execute conn "DELETE FROM posts WHERE id=(?)" $ (Only postId')
          pure "Post is deleted"