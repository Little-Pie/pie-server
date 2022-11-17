{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Image where

import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Database.PostgreSQL.Simple.Types (Query)
import Types.Entities.Image (Image)

getImageById :: Connection -> Int -> IO [Image]
getImageById conn imageId =
  query
    conn
    "SELECT * FROM images WHERE id=(?)"
    (Only imageId)

getImagesByPostIds :: Connection -> [Int] -> IO [Image]
getImagesByPostIds conn postIds =
  case addPostFilters initQuery postIds of
    Nothing -> pure []
    Just imagesQuery -> query conn imagesQuery postIds

initQuery :: Query
initQuery = "SELECT * FROM images"

addPostFilters :: Query -> [Int] -> Maybe Query
addPostFilters _ [] = Nothing
addPostFilters someQuery postIds = helper (someQuery <> " WHERE ") postIds
  where
    helper q [] = Just q
    helper q [_] = Just (q <> "\"postId\" = (?)")
    helper q (_ : xs) = helper (q <> "\"postId\" = (?) OR ") xs
