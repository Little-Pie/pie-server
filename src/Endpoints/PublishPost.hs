{-# LANGUAGE OverloadedStrings #-}

module Endpoints.PublishPost where

import Types.Entities.Post
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple

publishPost :: Connection -> Int -> IO (LBS.ByteString)
publishPost conn postId' = do
    posts <- query_ conn "select * from posts order by id"
    case postId' `elem` (map postId posts) of
        False -> pure "There are no posts with such id"
        True -> do
          mapM putStrLn $ map show $ map postId posts
          execute conn "UPDATE posts SET is_published = (?) WHERE id = (?)" $ (True,postId')
          pure "Post is published"