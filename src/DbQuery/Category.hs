{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Category where

import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (..),
    Query,
    execute,
    query,
  )
import Types.Entities.Category (Category)

getCategoryById :: Connection -> Int -> IO [Category]
getCategoryById conn categoryId =
  query
    conn
    "SELECT * FROM categories WHERE id=(?)"
    (Only categoryId)

getGeneralCategoryByName :: Connection -> String -> IO [Category]
getGeneralCategoryByName conn name =
  query
    conn
    "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=null"
    (Only name)

insertNewGeneralCategory :: Connection -> String -> IO ()
insertNewGeneralCategory conn name =
  void $
    execute
      conn
      "INSERT INTO categories (name) VALUES (?)"
      (Only name)

getCategoryByNameAndParent :: Connection -> String -> Int -> IO [Category]
getCategoryByNameAndParent conn name parentCategoryId =
  query
    conn
    "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=(?)"
    (name, parentCategoryId)

insertNewCategory :: Connection -> String -> Int -> IO ()
insertNewCategory conn name parentCategoryId =
  void $
    execute
      conn
      "INSERT INTO categories (name,\"parentId\") VALUES (?,?)"
      (name, parentCategoryId)

showCategories :: Connection -> Int -> Int -> IO [Category]
showCategories conn limit offset =
  query
    conn
    "select * from categories limit (?) offset (?)"
    (limit, offset)

editCategory :: Connection -> String -> Maybe Int -> Int -> IO ()
editCategory conn name parentCategoryId categoryId =
  void $
    execute
      conn
      "UPDATE categories SET (name,\"parentId\") = (?,?) WHERE id = (?)"
      (name, parentCategoryId, categoryId)

getCategoryByParentId :: Connection -> [Int] -> IO [Category]
getCategoryByParentId conn ids = case mkQuery ids of
  "" -> pure []
  someQuery -> query conn ("SELECT * FROM categories WHERE " <> someQuery) ids
  where
    mkQuery :: [Int] -> Query
    mkQuery [] = ""
    mkQuery [_] = " \"parentId\" = (?)"
    mkQuery (_ : xs) = " \"parentId\" = (?) OR " <> mkQuery xs
