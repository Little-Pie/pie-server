{-# LANGUAGE OverloadedStrings #-}

module DbQuery.User where

import Types.Entities.User
import Database.PostgreSQL.Simple

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn "select * from users"

getUserById :: Connection -> Int -> IO [User]
getUserById conn userId = query conn "SELECT * FROM users WHERE id=(?)" (Only userId)

insertNewUser :: Connection -> String -> String -> String -> Bool -> Bool -> IO ()
insertNewUser conn name login password isAdmin isAuthor = do
  execute conn "INSERT INTO users (name,login,password,\"isAdmin\",\"isAuthor\") VALUES (?,?,?,?,?)" (name,login,password,isAdmin,isAuthor)
  pure ()

showUsers :: Connection -> Int -> Int -> IO [User]
showUsers conn limit offset = query conn "select * from users limit (?) offset (?)" (limit, offset)