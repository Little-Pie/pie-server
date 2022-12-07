module DbQuery.User where

import Config (App)
import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Only (..),
    execute,
    query,
    query_,
  )
import Helpers (withDbConnection)
import Types.API.CreateUser (CreateUserRequest)
import Types.Entities.User (User)

getUsers :: App [User]
getUsers =
  withDbConnection (`query_` "select * from users")

getUserByLogin :: String -> App [User]
getUserByLogin login =
  withDbConnection
    ( \conn ->
        query
          conn
          "SELECT * FROM users WHERE login=(?)"
          (Only login)
    )

insertNewUser :: CreateUserRequest -> App ()
insertNewUser req = do
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "INSERT INTO users (name,login,password,\"isAdmin\",\"isAuthor\") VALUES (?,?,?,?,?)"
            req
      )

showUsers :: Int -> Int -> App [User]
showUsers limit offset =
  withDbConnection
    ( \conn ->
        query
          conn
          "select * from users limit (?) offset (?)"
          (limit, offset)
    )
