{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreateUser where

import qualified Types.Entities.User as U
import qualified DbQuery.User as DB
import Types.API.CreateUser
import qualified Data.ByteString.Lazy as LBS
import Network.Wai (Response)
import Database.PostgreSQL.Simple
import Helpers
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), createUserHandler)

createUser :: Connection -> U.User -> CreateUserRequest -> IO Response
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
