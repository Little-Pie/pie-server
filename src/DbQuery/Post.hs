{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DbQuery.Post where

import Config (App)
import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
  ( Only (..),
    execute,
    executeMany,
    query,
    returning,
  )
import Database.PostgreSQL.Simple.Types (Query (..))
import Helpers (withDbConnection)
import Types.Entities.GetPosts (GetPosts)
import Types.Entities.Post (Post)

insertNewPost ::
  String ->
  String ->
  Int ->
  Int ->
  Bool ->
  [String] ->
  [String] ->
  App ()
insertNewPost title text categoryId userId isPublished base64Images contentTypes = do
  postId' <-
    mapM (pure . fromOnly)
      =<< withDbConnection
        ( \conn ->
            returning
              conn
              "INSERT INTO posts (title,text,\"authorId\",\"isPublished\",\"categoryId\") \
              \VALUES (?,?,?,?,?) RETURNING id"
              [(title, text, userId, isPublished, categoryId)]
        ) ::
      App [Int]
  case postId' of
    [] -> liftIO $ putStrLn "Something went wrong: post wasn't created"
    (postId : _) -> do
      let imageRows = zipWith (postId,,) base64Images contentTypes
      void $
        withDbConnection
          ( \conn ->
              executeMany
                conn
                "INSERT INTO images (\"postId\",\"base64Image\",\"contentType\") VALUES (?,?,?)"
                imageRows
          )

editPost :: String -> String -> Int -> Int -> Bool -> [String] -> [String] -> App ()
editPost title text categoryId postId isPublished base64Images contentTypes = do
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "UPDATE posts SET (title,text,\"isPublished\",\"categoryId\") = (?,?,?,?) WHERE id = (?)"
            (title, text, isPublished, categoryId, postId)
      )
  case base64Images of
    [] -> pure ()
    _ -> do
      let imageRows = zipWith (postId,,) base64Images contentTypes
      void $
        withDbConnection
          ( \conn ->
              execute
                conn
                "DELETE FROM images WHERE \"postId\" = (?)"
                (Only postId)
          )
      void $
        withDbConnection
          ( \conn ->
              executeMany
                conn
                "INSERT INTO images (\"postId\",\"base64Image\",\"contentType\") VALUES (?,?,?)"
                imageRows
          )

getPostById :: Int -> App [Post]
getPostById postId =
  withDbConnection
    ( \conn ->
        query
          conn
          "select * from posts where id=(?)"
          (Only postId)
    )

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
  Int ->
  Int ->
  [(BS.ByteString, BS.ByteString)] ->
  Maybe BS.ByteString ->
  Maybe BS.ByteString ->
  App [GetPosts]
showPosts limit' offset' queryFilters mbQuerySortBy mbSearch = do
  withDbConnection
    ( \conn ->
        query
          conn
          ( initQuery <> getFilterBy queryFilters
              <> getSearch mbSearch
              <> getSortBy mbQuerySortBy
              <> " limit (?) offset (?)"
          )
          (limit', offset')
    )
