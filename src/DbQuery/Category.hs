{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Category where

import Config (App)
import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Only (..),
    Query,
    execute,
    query,
  )
import Helpers (withDbConnection)
import Types.Entities.Category (Category)

getCategoryById :: Int -> App [Category]
getCategoryById categoryId =
  withDbConnection
    ( \conn ->
        query
          conn
          "SELECT * FROM categories WHERE id=(?)"
          (Only categoryId)
    )

getGeneralCategoryByName :: String -> App [Category]
getGeneralCategoryByName name =
  withDbConnection
    ( \conn ->
        query
          conn
          "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=null"
          (Only name)
    )

insertNewGeneralCategory :: String -> App ()
insertNewGeneralCategory name =
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "INSERT INTO categories (name) VALUES (?)"
            (Only name)
      )

getCategoryByNameAndParent :: String -> Int -> App [Category]
getCategoryByNameAndParent name parentCategoryId =
  withDbConnection
    ( \conn ->
        query
          conn
          "SELECT * FROM categories WHERE name=(?) AND \"parentId\"=(?)"
          (name, parentCategoryId)
    )

insertNewCategory :: String -> Int -> App ()
insertNewCategory name parentCategoryId =
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "INSERT INTO categories (name,\"parentId\") VALUES (?,?)"
            (name, parentCategoryId)
      )

showCategories :: Int -> Int -> App [Category]
showCategories limit offset =
  withDbConnection
    ( \conn ->
        query
          conn
          "select * from categories limit (?) offset (?)"
          (limit, offset)
    )

editCategory :: String -> Maybe Int -> Int -> App ()
editCategory name parentCategoryId categoryId =
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "UPDATE categories SET (name,\"parentId\") = (?,?) WHERE id = (?)"
            (name, parentCategoryId, categoryId)
      )

getCategoryByParentId :: [Int] -> App [Category]
getCategoryByParentId ids = case mkQuery ids of
  "" -> pure []
  someQuery ->
    withDbConnection
      ( \conn ->
          query
            conn
            ("SELECT * FROM categories WHERE " <> someQuery)
            ids
      )
  where
    mkQuery :: [Int] -> Query
    mkQuery [] = ""
    mkQuery [_] = " \"parentId\" = (?)"
    mkQuery (_ : xs) = " \"parentId\" = (?) OR " <> mkQuery xs
