{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.CreateUser where

import Config (Environment (..))
import qualified DbQuery.User as DB
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), createUserHandler)
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreateUser as API
import Types.Entities.User (User)

createUser :: Environment -> User -> API.CreateUserRequest -> IO Response
createUser env@Environment {..} user req = do
  res <- createUserHandler handle user req
  case res of
    Success -> responseOk env "User is created"
    LoginIsTaken -> responseBadRequest env "User with such login already exists"
    NotFound -> responseNotFound env ""
  where
    handle =
      Handle
        { insertNewUser = DB.insertNewUser conn,
          getUserByLogin = DB.getUserByLogin conn
        }
