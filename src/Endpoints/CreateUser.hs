{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import Types.Entities.User (User)
import qualified DbQuery.User as DB
import qualified Types.API.CreateUser as API
import Network.Wai (Response)
import Database.PostgreSQL.Simple (Connection)
import Helpers (responseOk, responseBadRequest, responseNotFound)
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), createUserHandler)

createUser :: Connection -> User -> API.CreateUserRequest -> IO Response
createUser conn user req = do
  res <- createUserHandler handle user req
  case res of
    Success -> pure $ responseOk "User is created"
    LoginIsTaken -> pure $ responseBadRequest "User with such login already exists"
    NotFound -> pure $ responseNotFound ""
  where
    handle = Handle
      { insertNewUser = DB.insertNewUser conn,
        getUserByLogin = DB.getUserByLogin conn
      }
