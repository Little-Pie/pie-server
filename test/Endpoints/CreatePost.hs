{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import Data.Functor.Identity (Identity)
import Endpoints.Handlers.CreatePost
  ( CreatePostResult (..),
    Handle (..),
    createPostHandler,
  )
import Fixtures
  ( category,
    userAdminAuthor,
    userAdminNotAuthor,
  )
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Types.API.CreatePost (CreatePostRequest (..))
import Types.Entities.Category (Category (..))
import Types.Entities.Post (Post (..))
import Types.Entities.User (User (..))

handle :: Handle Identity
handle =
  Handle
    { getCategoryById = \_ -> pure [category],
      insertNewPost = \_ _ _ _ _ _ _ -> pure ()
    }

createPostRequest :: CreatePostRequest
createPostRequest =
  CreatePostRequest
    "title"
    "text"
    1
    False
    ["imageInBase64"]
    ["png"]

createPostTest :: SpecWith ()
createPostTest =
  describe "Post creation tests" $ do
    it "Should successfuly create post when requested by author" $ do
      let res =
            createPostHandler
              handle
              userAdminAuthor
              createPostRequest
      res `shouldBe` pure Success
    it "Should return bad request in case category with such id does not exists" $ do
      let res =
            createPostHandler
              handle {getCategoryById = \_ -> pure []}
              userAdminAuthor
              createPostRequest
      res `shouldBe` pure CategoryNotExist
    it "Should return bad request in case user not an author" $ do
      let res =
            createPostHandler
              handle
              userAdminNotAuthor {Types.Entities.User.isAuthor = False}
              createPostRequest
      res `shouldBe` pure NotAuthor
    it "Should return bad request in case image format is not supported" $ do
      let res =
            createPostHandler
              handle
              userAdminNotAuthor
              createPostRequest {contentTypes = [""]}
      res `shouldBe` pure NotAuthor
