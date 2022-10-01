{-# LANGUAGE OverloadedStrings #-}

module DbQuery.User where

import Types.Entities.User
import Database.PostgreSQL.Simple

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn "select * from users"

getUserById :: Connection -> Int -> IO [User]
getUserById conn userId = query conn "SELECT * FROM users WHERE id=(?)" (Only userId)

insertNewUser :: Connection -> String -> String -> String -> IO ()
insertNewUser conn name login password = do
  execute conn "INSERT INTO users (name,login,password,\"isAdmin\",\"isAuthor\") VALUES (?,?,?,?,?)" (name,login,password,False,False)
  pure ()

editUser :: Connection -> String -> String -> String -> Int -> IO ()
editUser conn name login password userId = do
  execute conn "UPDATE users SET (name,login,password) = (?,?,?) WHERE id = (?)" (name,login,password,userId)
  pure ()

deleteUser :: Connection -> Int ->  IO ()
deleteUser conn userId = do
  execute conn "DELETE FROM users WHERE id=(?)" $ (Only userId)
  pure ()

makeAdmin :: Connection -> Int -> IO ()
makeAdmin conn userId = do
  execute conn "UPDATE users SET \"isAdmin\" = (?) WHERE id = (?)" $ (True,userId)
  pure ()

makeAuthor :: Connection -> Int -> IO ()
makeAuthor conn userId = do
  execute conn "UPDATE users SET \"isAuthor\" = (?) WHERE id = (?)" $ (True,userId)
  pure ()

removeAdmin :: Connection -> Int -> IO ()
removeAdmin conn userId = do
  execute conn "UPDATE users SET \"isAdmin\" = (?) WHERE id = (?)" $ (False,userId)
  pure ()

removeAuthor :: Connection -> Int -> IO ()
removeAuthor conn userId = do
  execute conn "UPDATE users SET \"isAuthor\" = (?) WHERE id = (?)" $ (False,userId)
  pure ()

showUsers :: Connection -> Int -> Int -> IO [User]
showUsers conn limit offset = query conn "select * from users limit (?) offset (?)" (limit, offset)