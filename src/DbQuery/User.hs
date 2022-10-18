{-# LANGUAGE OverloadedStrings #-}

module DbQuery.User where

import Types.Entities.User (User)
import Database.PostgreSQL.Simple (Connection, Only(..), query_, query, execute)

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn "select * from users"

getUserByLogin :: Connection -> String -> IO [User]
getUserByLogin conn login = query conn "SELECT * FROM users WHERE login=(?)" (Only login)

insertNewUser :: Connection -> String -> String -> String -> Bool -> Bool -> IO ()
insertNewUser conn name login password isAdmin isAuthor = do
  execute conn "INSERT INTO users (name,login,password,\"isAdmin\",\"isAuthor\") VALUES (?,?,?,?,?)" (name,login,password,isAdmin,isAuthor)
  pure ()

showUsers :: Connection -> Int -> Int -> IO [User]
showUsers conn limit offset = query conn "select * from users limit (?) offset (?)" (limit, offset)
