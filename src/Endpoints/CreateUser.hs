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
import Endpoints.Handlers.CreateUser (Handle (..), createUserHandler)

createUser :: Connection -> Int -> CreateUserRequest -> IO Response
createUser conn userId req =
  createUserHandler handle userId req
  where
    handle = Handle
      { getUserById = DB.getUserById conn,
        insertNewUser = DB.insertNewUser conn
      }
