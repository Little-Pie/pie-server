{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Post where

import qualified Data.ByteString.Char8 as BS
import qualified Types.API.GetPosts as API
import Types.Entities.Category
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple.Types (Query(..))
import Data.Maybe (fromMaybe)

insertNewPost :: Connection -> String -> String -> Int -> Int -> IO (LBS.ByteString)
insertNewPost conn title text categoryId userId = do
  execute conn "INSERT INTO posts (title,text,authorId,isPublished,categoryId) VALUES (?,?,?,?,?)" (title,text,userId,False,categoryId)
  pure "Post is created"

initQuery = "SELECT posts.*,users.name,categories.name FROM posts JOIN users ON posts.authorId = users.id JOIN categories ON posts.categoryId = categories.id WHERE isPublished = true"

getSortBy :: (Maybe BS.ByteString) -> Query
getSortBy mbSortBy = fromMaybe ("") ((\n -> if n `elem` sorts then (if n == "category" then " order by categories.name " else (if n == "author" then " order by users.name " else " order by " <> Query n)) else "") <$> mbSortBy)

sorts :: [BS.ByteString]
sorts = ["category","createdAt","title","author"]

getFilterBy :: [(BS.ByteString, BS.ByteString)] -> Query
getFilterBy queryFilters = mconcat $ map (\(filter',filterParam) -> createFilterDBReq filter' filterParam) queryFilters

createFilterDBReq :: BS.ByteString -> BS.ByteString -> Query
createFilterDBReq filt filterParam = case filt of
  "createdAt" -> " AND date(posts.createdAt) = '" <> Query filterParam <> "' "
  "createdUntil" -> " AND date(posts.createdAt) < '" <> Query filterParam <> "' "
  "createdSince" -> " AND date(posts.createdAt) > '" <> Query filterParam <> "' "
  "author" -> " AND users.name = '" <> Query filterParam <> "' "
  "categoryId" -> " AND categoryId = " <> Query filterParam <> " "
  "title" -> " AND title like '%" <> Query filterParam <> "%' "
  "text" -> " AND text like '%" <> Query filterParam <> "%' "

showPosts :: Connection -> Int -> Int -> [(BS.ByteString, BS.ByteString)] -> (Maybe BS.ByteString) -> IO [API.GetPosts]
showPosts conn limit' offset' queryFilters mbQuerySortBy = do
    query conn (initQuery <> (getFilterBy queryFilters) <> (getSortBy mbQuerySortBy) <> " limit (?) offset (?)") (limit', offset')

