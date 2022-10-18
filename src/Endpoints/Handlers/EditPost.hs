{-# LANGUAGE RecordWildCards #-}

module Endpoints.Handlers.EditPost where

import qualified Types.Entities.User as U
import qualified Types.Entities.Category as C
import qualified Types.Entities.Post as P
import Types.API.EditPost as API (EditPostRequest (..))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

data Handle m = Handle
  { editPost :: String -> String -> Int -> Int -> Bool -> [String] -> [String] -> m (),
    getCategoryById :: Int -> m [C.Category],
    getPostById :: Int -> m [P.Post]
  }

data EditPostResult = Success | PostNotExist | NotAuthor
  deriving (Eq, Show)

editPostHandler :: (Monad m) => Handle m -> U.User -> EditPostRequest -> m EditPostResult
editPostHandler Handle {..} user EditPostRequest {..} = do
  checkedCategoryId <-
    case categoryId of
      Just cId -> do
        categories <- getCategoryById cId
        case categories of
          [] -> pure Nothing
          _ -> pure categoryId
      Nothing -> pure Nothing
  posts <- getPostById postId
  case posts of
    [] -> pure PostNotExist
    (post:_) -> do
      if U.userId user == P.authorId post
        then do
          let newPost = post {
            P.title = fromMaybe (P.title post) title,
            P.text = fromMaybe (P.text post) text,
            P.categoryId = fromMaybe (P.categoryId post) checkedCategoryId,
            P.isPublished = fromMaybe (P.isPublished post) isPublished}
          let base64Images' = fromMaybe [] base64Images
          let contentTypes' = fromMaybe [] contentTypes
          editPost (P.title newPost) (P.text newPost) (P.categoryId newPost) postId (P.isPublished newPost) base64Images' contentTypes'
          pure Success
        else pure NotAuthor
