{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.CreatePost where

import Types.API.CreatePost (CreatePostRequest (..))
import qualified Types.Entities.Category as C
import qualified Types.Entities.User as U

data Handle m = Handle
  { getCategoryById :: Int -> m [C.Category],
    insertNewPost :: CreatePostRequest -> Int -> m ()
  }

data CreatePostResult = Success | CategoryNotExist | NotAuthor | WrongContentType
  deriving (Eq, Show)

createPostHandler ::
  (Monad m) =>
  Handle m ->
  U.User ->
  CreatePostRequest ->
  m CreatePostResult
createPostHandler Handle {..} author req@CreatePostRequest {..}
  | U.isAuthor author = do
    category <- getCategoryById categoryId
    case category of
      [] -> pure CategoryNotExist
      _ -> do
        if all (`elem` ["png", "jpg", "jpeg"]) contentTypes
          then do
            insertNewPost req (U.userId author)
            pure Success
          else pure WrongContentType
  | otherwise = pure NotAuthor
