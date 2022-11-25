{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module DbQuery.Post where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
  ( Connection,
    Only (..),
    execute,
    executeMany,
    query,
    returning,
  )
import Database.PostgreSQL.Simple.Types (Query (..))
import Types.Db (EditPost (..), InsertNewPost (..))
import Types.Entities.GetPosts (GetPosts)
import Types.Entities.Post (Post)

insertNewPost :: Connection -> InsertNewPost -> IO ()
insertNewPost conn InsertNewPost {..} = do
  postId' <-
    mapM (pure . fromOnly)
      =<< returning
        conn
        "INSERT INTO posts (title,text,\"authorId\",\"isPublished\",\"categoryId\") \
        \VALUES (?,?,?,?,?) RETURNING id"
        [(title, text, userId, isPublished, categoryId)] ::
      IO [Int]
  case postId' of
    [] -> putStrLn "Something went wrong: post wasn't created"
    (postId : _) -> do
      let imageRows = zipWith (postId,,) base64Images contentTypes
      void $
        executeMany
          conn
          "INSERT INTO images (\"postId\",\"base64Image\",\"contentType\") VALUES (?,?,?)"
          imageRows

editPost :: Connection -> EditPost -> IO ()
editPost conn EditPost {..} = do
  void $
    execute
      conn
      "UPDATE posts SET (title,text,\"isPublished\",\"categoryId\") = (?,?,?,?) WHERE id = (?)"
      (title, text, isPublished, categoryId, postId)
  case base64Images of
    [] -> pure ()
    _ -> do
      let imageRows = zipWith (postId,,) base64Images contentTypes
      void $
        execute
          conn
          "DELETE FROM images WHERE \"postId\" = (?)"
          (Only postId)
      void $
        executeMany
          conn
          "INSERT INTO images (\"postId\",\"base64Image\",\"contentType\") VALUES (?,?,?)"
          imageRows

getPostById :: Connection -> Int -> IO [Post]
getPostById conn postId =
  query
    conn
    "select * from posts where id=(?)"
    (Only postId)

initQuery :: Query
initQuery =
  "SELECT posts.*,users.name,categories.name \
  \FROM posts JOIN users ON posts.\"authorId\" = users.id \
  \JOIN categories ON posts.\"categoryId\" = categories.id \
  \LEFT OUTER JOIN images ON posts.id = images.\"postId\" \
  \WHERE \"isPublished\" = true "

getSortBy :: Maybe BS.ByteString -> Query
getSortBy =
  maybe
    ""
    ( \case
        "category" -> " order by categories.name "
        "author" -> " order by users.name "
        "createdAt" -> " order by createdAt "
        "title" -> " order by title "
        "imagesNumber" ->
          " GROUP BY posts.id, users.name, categories.name \
          \ORDER BY COUNT(images.\"postId\") DESC "
        _ -> ""
    )

getSearch :: Maybe BS.ByteString -> Query
getSearch =
  maybe
    ""
    ( \n ->
        " AND (title like '%"
          <> Query n
          <> "%' OR text like '%"
          <> Query n
          <> "%' OR users.name like '%"
          <> Query n
          <> "%' OR categories.name like '%"
          <> Query n
          <> "%')"
    )

getFilterBy :: [(BS.ByteString, BS.ByteString)] -> Query
getFilterBy queryFilters =
  mconcat $
    map (uncurry createFilterDBReq) queryFilters

createFilterDBReq :: BS.ByteString -> BS.ByteString -> Query
createFilterDBReq filt filterParam = case filt of
  "createdAt" ->
    " AND date(\"posts.createdAt\") = '"
      <> Query filterParam
      <> "' "
  "createdUntil" ->
    " AND date(\"posts.createdAt\") < '"
      <> Query filterParam
      <> "' "
  "createdSince" ->
    " AND date(\"posts.createdAt\") > '"
      <> Query filterParam
      <> "' "
  "author" ->
    " AND users.name = '"
      <> Query filterParam
      <> "' "
  "categoryId" ->
    " AND \"categoryId\" = "
      <> Query filterParam
      <> " "
  "title" ->
    " AND title like '%"
      <> Query filterParam
      <> "%' "
  "text" ->
    " AND text like '%"
      <> Query filterParam
      <> "%' "
  _ -> ""

showPosts ::
  Connection ->
  Int ->
  Int ->
  [(BS.ByteString, BS.ByteString)] ->
  Maybe BS.ByteString ->
  Maybe BS.ByteString ->
  IO [GetPosts]
showPosts conn limit' offset' queryFilters mbQuerySortBy mbSearch = do
  print
    ( initQuery <> getFilterBy queryFilters
        <> getSearch mbSearch
        <> getSortBy mbQuerySortBy
        <> " limit (?) offset (?)"
    )
  query
    conn
    ( initQuery <> getFilterBy queryFilters
        <> getSearch mbSearch
        <> getSortBy mbQuerySortBy
        <> " limit (?) offset (?)"
    )
    (limit', offset')
