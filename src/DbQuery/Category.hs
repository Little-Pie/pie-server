{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Category where

import Types.Entities.Category (Category)
import Database.PostgreSQL.Simple (Connection, Query, Only(..), query, execute)

getCategoryById :: Connection -> Int -> IO [Category]
getCategoryById conn categoryId =
  query conn "SELECT * FROM categories WHERE id=(?)" (Only categoryId)

getGeneralCategoryByName :: Connection -> String -> IO [Category]
getGeneralCategoryByName conn name =
  query conn "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=null" (Only name)

insertNewGeneralCategory :: Connection -> String -> IO ()
insertNewGeneralCategory conn name = do
  execute conn "INSERT INTO categories (name) VALUES (?)" (Only name)
  pure ()

getCategoryByNameAndParent ::  Connection -> String -> Int -> IO [Category]
getCategoryByNameAndParent conn name parentCategoryId = do
  query conn "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=(?)" (name,parentCategoryId)

insertNewCategory :: Connection -> String -> Int -> IO ()
insertNewCategory conn name parentCategoryId = do
  execute conn "INSERT INTO categories (name,\"parentId\") VALUES (?,?)" (name,parentCategoryId)
  pure ()

showCategories :: Connection -> Int -> Int -> IO [Category]
showCategories conn limit offset = query conn "select * from categories limit (?) offset (?)" (limit, offset)

editCategory :: Connection -> String -> Maybe Int -> Int -> IO ()
editCategory conn name parentCategoryId categoryId = do
  execute conn "UPDATE categories SET (name,\"parentId\") = (?,?) WHERE id = (?)" (name,parentCategoryId,categoryId)
  pure ()

getCategoryByParentId :: Connection -> [Int] -> IO [Category]
getCategoryByParentId conn ids = do
  case mkQuery ids of
    "" -> pure []
    someQuery -> query conn ("SELECT * FROM categories WHERE " <> someQuery) ids
  where
    mkQuery :: [Int] -> Query
    mkQuery [] = ""
    mkQuery [x] = " \"parentId\" = (?)"
    mkQuery (x:xs) = " \"parentId\" = (?) OR "
