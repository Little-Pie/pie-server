{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreateUser where

import qualified Types.Entities.User as U
import Types.API.CreateUser
import Network.Wai (Response)
import Helpers

data Handle m = Handle
  { insertNewUser :: String -> String -> String -> Bool -> Bool -> m ()
  }

createUserHandler :: (Monad m) => Handle m -> U.User -> CreateUserRequest -> m Response
createUserHandler Handle {..} user CreateUserRequest {..} = do
  if U.isAdmin user
    then do
      insertNewUser name login password isAdmin isAuthor
      pure $ responseOk "User is created"
    else pure $ responseNotFound ""
