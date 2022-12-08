module Endpoints.EditPost where

import Config (App)
import qualified DbQuery.Category as DBC
import qualified DbQuery.Post as DBP
import Endpoints.Handlers.EditPost
  ( EditPostResult (..),
    Handle (..),
    editPostHandler,
  )
import Helpers (responseBadRequest, responseNotFound, responseOk)
import Network.Wai (Response)
import Types.API.EditPost (EditPostRequest)
import Types.Entities.User (User)

editPost :: User -> EditPostRequest -> App Response
editPost user req = do
  res <- editPostHandler handle user req
  case res of
    Success ->
      responseOk
        "Changes applied"
    PostNotExist ->
      responseBadRequest
        "Post with such id does not exist"
    NotAuthor ->
      responseNotFound
        ""
  where
    handle =
      Handle
        { Endpoints.Handlers.EditPost.editPost = DBP.editPost,
          getCategoryById = DBC.getCategoryById,
          getPostById = DBP.getPostById
        }
