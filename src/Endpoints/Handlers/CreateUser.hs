module Endpoints.Handlers.CreateUser where

import Hash (makeStringHash)
import Types.API.CreateUser (CreateUserRequest (..))
import qualified Types.Entities.User as U

data Handle m = Handle
  { insertNewUser :: CreateUserRequest -> m (),
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
createUserHandler Handle {..} user req@CreateUserRequest {..} =
  if U.isAdmin user
    then do
      mbUser <- getUserByLogin login
      case mbUser of
        [] -> do
          insertNewUser req {password = makeStringHash password}
          pure Success
        _ -> pure LoginIsTaken
    else pure NotFound
