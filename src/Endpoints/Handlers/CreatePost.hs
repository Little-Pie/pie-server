{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreatePost where

import qualified Types.Entities.User as U
import qualified Types.Entities.Category as C
import Types.API.CreatePost (CreatePostRequest (..))

data Handle m = Handle
  { getCategoryById :: Int -> m [C.Category],
    insertNewPost :: String -> String -> Int -> Int -> Bool -> [String] -> [String] -> m ()
  }

data CreatePostResult = Success | CategoryNotExist | NotAuthor
  deriving (Eq, Show)

createPostHandler :: (Monad m) => Handle m -> U.User -> CreatePostRequest -> m CreatePostResult
createPostHandler Handle {..} author CreatePostRequest {..} =
  if U.isAuthor author
    then do
      category <- getCategoryById categoryId
      case category of
        [] -> pure CategoryNotExist
        _ -> do
          insertNewPost title text categoryId (U.userId author) isPublished base64Images contentTypes
          pure Success
    else pure NotAuthor
