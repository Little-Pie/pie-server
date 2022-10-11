{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Image where

import Types.Entities.Image
import Database.PostgreSQL.Simple

getImageById :: Connection -> Int -> IO [Image]
getImageById conn imageId =
  query conn "SELECT * FROM images WHERE id=(?)" (Only imageId)

getImagesByPostIds :: Connection -> [Int] -> IO [Image]
getImagesByPostIds conn postIds =
  case addPostFilters initQuery postIds of
    Nothing -> pure []
    Just imagesQuery -> query conn imagesQuery postIds

initQuery = "SELECT * FROM images"

addPostFilters q [] = Nothing
addPostFilters q postIds = helper (q <> " WHERE ") postIds
  where
    helper q [] = Just q
    helper q [postId] = Just (q <> "\"postId\" = (?)")
    helper q (postId:xs) = helper (q <> "\"postId\" = (?) OR ") xs