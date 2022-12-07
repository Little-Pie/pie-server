module Endpoints.CreateUser where

import Config (App)
import qualified DbQuery.User as DB
import Endpoints.Handlers.CreateUser
  ( CreateUserResult (..),
    Handle (..),
    createUserHandler,
  )
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import qualified Types.API.CreateUser as API
import Types.Entities.User (User)

createUser :: User -> API.CreateUserRequest -> App Response
createUser user req = do
  res <- createUserHandler handle user req
  case res of
    Success ->
      responseOk
        "User is created"
    LoginIsTaken ->
      responseBadRequest
        "User with such login already exists"
    NotFound ->
      responseNotFound
        ""
  where
    handle =
      Handle
        { insertNewUser = DB.insertNewUser,
          getUserByLogin = DB.getUserByLogin
        }
