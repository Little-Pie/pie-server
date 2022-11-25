{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreateUser where

import Hash (makeStringHash)
import Types.API.CreateUser (CreateUserRequest (..))
import Types.Db (InsertNewUser (..))
import qualified Types.Entities.User as U

data Handle m = Handle
  { insertNewUser :: InsertNewUser -> m (),
    getUserByLogin :: String -> m [U.User]
  }

data CreateUserResult = Success | LoginIsTaken | NotFound
  deriving (Eq, Show)

createUserHandler ::
  (Monad m) =>
  Handle m ->
  U.User ->
  CreateUserRequest ->
  m CreateUserResult
createUserHandler Handle {..} user CreateUserRequest {..} =
  if U.isAdmin user
    then do
      mbUser <- getUserByLogin login
      case mbUser of
        [] -> do
          insertNewUser
            ( InsertNewUser
                name
                login
                (makeStringHash password)
                isAdmin
                isAuthor
            )
          pure Success
        _ -> pure LoginIsTaken
    else pure NotFound
