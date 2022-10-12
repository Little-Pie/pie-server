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

createUser :: Connection -> U.User -> CreateUserRequest -> IO Response
createUser conn user req =
  createUserHandler handle user req
  where
    handle = Handle
      { insertNewUser = DB.insertNewUser conn
      }
