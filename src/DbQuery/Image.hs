module DbQuery.Image where

import Config (App)
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.Types (Query)
import Helpers (withDbConnection)
import Types.Entities.Image (Image)

getImageById :: Int -> App [Image]
getImageById imageId =
  withDbConnection
    ( \conn ->
        query
          conn
          "SELECT * FROM images WHERE id=(?)"
          (Only imageId)
    )

getImagesByPostIds :: [Int] -> App [Image]
getImagesByPostIds postIds =
  case addPostFilters initQuery postIds of
    Nothing -> pure []
    Just imagesQuery ->
      withDbConnection
        ( \conn ->
            query
              conn
              imagesQuery
              postIds
        )

initQuery :: Query
initQuery = "SELECT * FROM images"

addPostFilters :: Query -> [Int] -> Maybe Query
addPostFilters _ [] = Nothing
addPostFilters someQuery postIds = helper (someQuery <> " WHERE ") postIds
  where
    helper q [] = Just q
    helper q [_] = Just (q <> "\"postId\" = (?)")
    helper q (_ : xs) = helper (q <> "\"postId\" = (?) OR ") xs
