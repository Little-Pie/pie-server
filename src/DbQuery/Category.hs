{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Category where

import Types.Entities.Category
import Database.PostgreSQL.Simple

getCategoryById :: Connection -> Int -> IO [Category]
getCategoryById conn categoryId =
  query conn "SELECT * FROM categories WHERE id=(?)" (Only categoryId) :: IO [Category]

getGeneralCategoryByName :: Connection -> String -> IO [Category]
getGeneralCategoryByName conn name =
  query conn "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=null" (Only name) :: IO [Category]

insertNewGeneralCategory :: Connection -> String -> IO ()
insertNewGeneralCategory conn name = do
  execute conn "INSERT INTO categories (name) VALUES (?)" (Only name)
  pure ()

getCategoryByNameAndParent ::  Connection -> String -> Int -> IO [Category]
getCategoryByNameAndParent conn name parentCategoryId = do
  query conn "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=(?)" (name,parentCategoryId) :: IO [Category]

insertNewCategory :: Connection -> String -> Int -> IO ()
insertNewCategory conn name parentCategoryId = do
  execute conn "INSERT INTO categories (name,\"parentId\") VALUES (?,?)" (name,parentCategoryId)
  pure ()

showCategories :: Connection -> Int -> Int -> IO [Category]
showCategories conn limit offset = query conn "select * from categories limit (?) offset (?)" (limit, offset)