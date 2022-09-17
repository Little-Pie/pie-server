{-# LANGUAGE OverloadedStrings #-}

module DbQuery.ShowPosts where

import qualified Data.ByteString.Char8 as BS
import qualified Types.API.GetPosts as API
import Types.Entities.Category
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Data.Maybe (fromMaybe)

initQuery = "SELECT posts.*,users.name,categories.name FROM posts JOIN users ON posts.author_id = users.id JOIN categories ON posts.category_id = categories.id WHERE is_published = true"

getSortBy :: (Maybe BS.ByteString) -> Query
getSortBy mbSortBy = fromMaybe ("") ((\n -> if n `elem` sorts then (if n == "category" then " order by categories.name " else (if n == "author" then " order by users.name " else " order by " <> Query n)) else "") <$> mbSortBy)

sorts :: [BS.ByteString]
sorts = ["category","created_at","title","author"]

getFilterBy :: [(BS.ByteString, BS.ByteString)] -> Query
getFilterBy queryFilters = mconcat $ map (\(filter',filterParam) -> createFilterDBReq filter' filterParam) queryFilters

createFilterDBReq :: BS.ByteString -> BS.ByteString -> Query
createFilterDBReq filt filterParam = case filt of
  "created_at" -> " AND date(posts.created_at) = '" <> Query filterParam <> "' "
  "created_until" -> " AND date(posts.created_at) < '" <> Query filterParam <> "' "
  "created_since" -> " AND date(posts.created_at) > '" <> Query filterParam <> "' "
  "author" -> " AND users.name = '" <> Query filterParam <> "' "
  "category_id" -> " AND category_id = " <> Query filterParam <> " "
  "title" -> " AND title like '%" <> Query filterParam <> "%' "
  "text" -> " AND text like '%" <> Query filterParam <> "%' "

showPosts :: Connection -> Int -> Int -> [(BS.ByteString, BS.ByteString)] -> (Maybe BS.ByteString) -> IO [API.GetPosts]
showPosts conn limit' offset' queryFilters mbQuerySortBy = do
    query conn (initQuery <> (getFilterBy queryFilters) <> (getSortBy mbQuerySortBy) <> " limit (?) offset (?)") (limit', offset')

