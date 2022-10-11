{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DbQuery.Post where

import Types.Entities.Post
import Types.Entities.GetPosts
import qualified Data.ByteString.Char8 as BS
import Types.Entities.Category
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple.Types (Query(..))
import Data.Maybe (fromMaybe)

insertNewPost :: Connection -> String -> String -> Int -> Int -> Bool -> [String] -> [String] -> IO ()
insertNewPost conn title text categoryId userId isPublished base64Images contentTypes = do
  postId' <- mapM (pure . fromOnly) =<< returning conn "INSERT INTO posts (title,text,\"authorId\",\"isPublished\",\"categoryId\") VALUES (?,?,?,?,?) RETURNING id" [(title,text,userId,isPublished,categoryId)] :: IO [Int]
  case postId' of
    [] -> putStrLn "Something went wrong: post wasn't created"
    (postId:_) -> do
      let imageRows = zipWith (postId,,) base64Images contentTypes
      executeMany conn "INSERT INTO images (postId,base64Image,contentType) VALUES (?,?,?)" imageRows
      pure ()

editPost :: Connection -> String -> String -> Int -> Int -> Bool -> [String] -> [String] -> IO ()
editPost conn title text categoryId postId isPublished base64Images contentTypes = do
  execute conn "UPDATE posts SET (title,text,\"isPublished\",\"categoryId\") = (?,?,?) WHERE id = (?)" (title, text, isPublished, categoryId, postId)
  case base64Images of
    [] -> pure ()
    _ -> do
      let imageRows = zipWith (postId,,) base64Images contentTypes
      execute conn "DELETE FROM images WHERE postId = (?)" (Only postId)
      executeMany conn "INSERT INTO images (postId,base64Image,contentType) VALUES (?,?,?)" imageRows
      pure ()

getPostById :: Connection -> Int -> IO [Post]
getPostById conn postId = query conn "select * from posts where id=(?)" (Only postId)

initQuery = "SELECT posts.*,users.name,categories.name FROM posts JOIN users ON posts.\"authorId\" = users.id JOIN categories ON posts.\"categoryId\" = categories.id LEFT OUTER JOIN images ON posts.id = images.\"postId\" WHERE \"isPublished\" = true "

getSortBy :: (Maybe BS.ByteString) -> Query
getSortBy mbSortBy = fromMaybe ("") ((\n -> case n of
    "category" -> " order by categories.name "
    "author" -> " order by users.name "
    "createdAt" -> " order by createdAt "
    "title" -> " order by title "
    "imagesNumber" -> " GROUP BY posts.id, users.name, categories.name ORDER BY COUNT(images.\"postId\") DESC "
    _ -> "") <$> mbSortBy)

getFilterBy :: [(BS.ByteString, BS.ByteString)] -> Query
getFilterBy queryFilters = mconcat $ map (\(filter',filterParam) -> createFilterDBReq filter' filterParam) queryFilters

createFilterDBReq :: BS.ByteString -> BS.ByteString -> Query
createFilterDBReq filt filterParam = case filt of
  "createdAt" -> " AND date(\"posts.createdAt\") = '" <> Query filterParam <> "' "
  "createdUntil" -> " AND date(\"posts.createdAt\") < '" <> Query filterParam <> "' "
  "createdSince" -> " AND date(\"posts.createdAt\") > '" <> Query filterParam <> "' "
  "author" -> " AND users.name = '" <> Query filterParam <> "' "
  "categoryId" -> " AND \"categoryId\" = " <> Query filterParam <> " "
  "title" -> " AND title like '%" <> Query filterParam <> "%' "
  "text" -> " AND text like '%" <> Query filterParam <> "%' "

showPosts :: Connection -> Int -> Int -> [(BS.ByteString, BS.ByteString)] -> (Maybe BS.ByteString) -> IO [GetPosts]
showPosts conn limit' offset' queryFilters mbQuerySortBy = do
  let q = (initQuery <> (getFilterBy queryFilters) <> (getSortBy mbQuerySortBy) <> " limit (?) offset (?)")
  print q
  query conn (initQuery <> (getFilterBy queryFilters) <> (getSortBy mbQuerySortBy) <> " limit (?) offset (?)") (limit', offset')
