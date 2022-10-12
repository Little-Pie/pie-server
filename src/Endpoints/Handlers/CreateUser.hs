{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreateUser where

import qualified Types.Entities.User as U
import Types.API.CreateUser (CreateUserRequest (..))

data Handle m = Handle
  { insertNewUser :: String -> String -> String -> Bool -> Bool -> m (),
    getUserByLogin :: String -> m [U.User]
  }

data CreateUserResult = Success | LoginIsTaken | NotFound
  deriving (Eq, Show)

createUserHandler :: (Monad m) => Handle m -> U.User -> CreateUserRequest -> m CreateUserResult
createUserHandler Handle {..} user CreateUserRequest {..} = do
  if U.isAdmin user
    then do
      mbUser <- getUserByLogin login
      case mbUser of
        [] -> do
          insertNewUser name login password isAdmin isAuthor
          pure Success
        _ -> pure LoginIsTaken
    else pure NotFound
