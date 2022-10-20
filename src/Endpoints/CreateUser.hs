{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import Database.PostgreSQL.Simple (Connection)
import qualified DbQuery.User as DB
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), createUserHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreateUser as API
import Types.Entities.User (User)

createUser :: Connection -> User -> API.CreateUserRequest -> IO Response
createUser conn user req = do
  res <- createUserHandler handle user req
  case res of
    Success -> pure $ responseOk "User is created"
    LoginIsTaken -> pure $ responseBadRequest "User with such login already exists"
    NotFound -> pure $ responseNotFound ""
  where
    handle =
      Handle
        { insertNewUser = DB.insertNewUser conn,
          getUserByLogin = DB.getUserByLogin conn
        }
