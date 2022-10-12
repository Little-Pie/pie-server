{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreateUser where

import qualified Types.Entities.User as U
import Types.API.CreateUser
import Network.Wai (Response)
import Helpers

data Handle m = Handle
  { getUserById :: Int -> m [U.User],
    insertNewUser :: String -> String -> String -> Bool -> Bool -> m ()
  }

createUserHandler :: (Monad m) => Handle m -> Int -> CreateUserRequest -> m Response
createUserHandler Handle{..} userId CreateUserRequest{..} = do
    admin <- getUserById userId
    case admin of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      (x:_) -> if U.isAdmin x
        then do
          insertNewUser name login password isAdmin isAuthor
          pure $ responseOk "User is created"
        else pure $ responseNotFound ""
