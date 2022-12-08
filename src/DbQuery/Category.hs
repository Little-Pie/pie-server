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
import Types.API.CreateCategory (CreateCategoryRequest)
import Types.Db (EditCategory)
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
          "SELECT * FROM categories WHERE name=(?) AND (\"parentId\" IS null)"
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

insertNewCategory :: CreateCategoryRequest -> App ()
insertNewCategory req =
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "INSERT INTO categories (name,\"parentId\") VALUES (?,?)"
            req
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

editCategory :: EditCategory -> App ()
editCategory req =
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "UPDATE categories SET (name,\"parentId\") = (?,?) WHERE id = (?)"
            req
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
