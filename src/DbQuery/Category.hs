{-# LANGUAGE OverloadedStrings #-}

module DbQuery.Category where

import Types.Entities.Category
import Database.PostgreSQL.Simple

getGeneralCategoryWithName :: Connection -> String -> IO [Category]
getGeneralCategoryWithName conn name' =
  query conn "select * from categories where name=(?) AND parent_id=null" (Only name') :: IO [Category]

insertNewGeneralCategory :: Connection -> String -> IO ()
insertNewGeneralCategory conn name' = do
  execute conn "INSERT INTO categories (name) VALUES (?)" $ (Only name')
  pure ()

getCategoryWithNameAndParent ::  Connection -> String -> Int -> IO [Category]
getCategoryWithNameAndParent conn name' parentCategoryId' = do
  putStrLn "1"
  query conn "select * from categories where name=(?) AND parent_id=(?)" $ (name',parentCategoryId') :: IO [Category]

insertNewCategory :: Connection -> String -> Int -> IO ()
insertNewCategory conn name' parentCategoryId' = do
  putStrLn "2"
  execute conn "INSERT INTO categories (name,parent_id) VALUES (?,?)" $ (name',parentCategoryId')
  putStrLn "3"
  pure ()