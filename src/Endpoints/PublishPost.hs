{-# LANGUAGE OverloadedStrings #-}

module Endpoints.PublishPost where

import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple

publishPost :: Connection -> Int -> IO (LBS.ByteString)
publishPost conn postId' = do
    posts <- query_ conn "select * from posts"
    case postId' `elem` (map postId posts) of
        False -> pure "There are no posts with such id"
        True -> do
          posts <- query conn "select * from posts order where id=(?)" $ (Only postId') :: IO [Post]
          case posts of
            [] -> pure "Something went wrong: empty list"
            [x] -> if isPublished x 
            then pure "The post is already published"
            else do
              mapM putStrLn $ map show $ map postId posts
              execute conn "UPDATE posts SET is_published = (?) WHERE id = (?)" $ (True,postId')
              pure "Post is published"
