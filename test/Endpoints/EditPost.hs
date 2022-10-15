{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditPost where

import Endpoints.Handlers.EditPost (EditPostResult (..), Handle (..), editPostHandler)
import Types.Entities.User (User (..))
import Types.Entities.Category (Category (..))
import Types.Entities.Post (Post (..))
import Types.API.EditPost (EditPostRequest (..))
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Fixtures (category,userAdminAuthor,userAdminNotAuthor,post)

handle :: Handle Identity
handle = Handle
  { editPost = \_ _ _ _ _ _ _ -> pure (),
    getCategoryById = \_ -> pure [category],
    getPostById = \_ -> pure [post]
  }

editPostRequest :: EditPostRequest
editPostRequest = EditPostRequest 1 (Just "title") (Just "text") (Just 1) (Just False) (Just ["imageInBase64"]) (Just ["imageContentType"])

editPostTest :: SpecWith ()
editPostTest =
  describe "Post editing tests" $ do
    it "Should successfuly edit post when requested by author" $ do
      let res = editPostHandler handle userAdminAuthor editPostRequest
      res `shouldBe` pure Success
    it "Should return bad request in case post with such id does not exists" $ do
      let res = editPostHandler handle {getPostById = \_ -> pure []} userAdminAuthor editPostRequest
      res `shouldBe` pure PostNotExist
    it "Should return bad request in case user not an author" $ do
      let res = editPostHandler handle userAdminNotAuthor editPostRequest
      res `shouldBe` pure NotAuthor