{-# LANGUAGE OverloadedStrings #-}

module DbQuery.User where

import Types.Entities.User
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy as LBS

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn "select * from users"

getUserById :: Connection -> Int -> IO [User]
getUserById conn userId =
  query conn "SELECT * FROM users WHERE id=(?)" (Only userId) :: IO [User]

insertNewUser :: Connection -> String -> String -> String -> IO (LBS.ByteString)
insertNewUser conn name login password = do
  execute conn "INSERT INTO users (name,login,password,\"isAdmin\",\"isAuthor\") VALUES (?,?,?,?,?)" (name,login,password,False,False)
  pure "User is created"

editUser :: Connection -> String -> String -> String -> Int -> IO (LBS.ByteString)
editUser conn name login password userId = do
  execute conn "UPDATE users SET (name,login,password) = (?,?,?) WHERE id = (?)" (name,login,password,userId)
  pure "Changes applied"

deleteUser :: Connection -> Int ->  IO (LBS.ByteString)
deleteUser conn userId = do
  execute conn "DELETE FROM users WHERE id=(?)" $ (Only userId)
  pure "User is deleted"

makeAdmin :: Connection -> Int -> IO (LBS.ByteString)
makeAdmin conn userId = do
  execute conn "UPDATE users SET \"isAdmin\" = (?) WHERE id = (?)" $ (True,userId)
  pure "Now this user is an admin"

makeAuthor :: Connection -> Int -> IO (LBS.ByteString)
makeAuthor conn userId = do
  execute conn "UPDATE users SET \"isAuthor\" = (?) WHERE id = (?)" $ (True,userId)
  pure "Now this user is an author"

removeAdmin :: Connection -> Int -> IO (LBS.ByteString)
removeAdmin conn userId = do
  execute conn "UPDATE users SET \"isAdmin\" = (?) WHERE id = (?)" $ (False,userId)
  pure "Now this user is not an admin"

removeAuthor :: Connection -> Int -> IO (LBS.ByteString)
removeAuthor conn userId = do
  execute conn "UPDATE users SET \"isAuthor\" = (?) WHERE id = (?)" $ (False,userId)
  pure "Now this user is not an author"