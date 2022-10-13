{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreatePost where

import Endpoints.Handlers.CreatePost (CreatePostResult (..), Handle (..), createPostHandler)
import Types.Entities.User (User (..))
import Types.Entities.Category (Category (..))
import Types.Entities.Post (Post (..))
import Types.API.CreatePost (CreatePostRequest (..))
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, it, shouldBe, SpecWith)

handle :: Handle Identity
handle = Handle
  { getCategoryById = \_ -> pure [category],
    insertNewPost = \_ _ _ _ _ _ _ -> pure ()
  }

user :: User
user = User 1 "name" "login" "password" undefined True True

category :: Category
category = Category 1 "name" Nothing

createPostRequest :: CreatePostRequest
createPostRequest = CreatePostRequest "title" "text" 1 False ["imageInBase64"] ["imageContentType"]

createPostTest :: SpecWith ()
createPostTest =
  describe "Post creation tests" $ do
    it "Should successfuly create post when requested by author" $ do
      let res = createPostHandler handle user createPostRequest
      res `shouldBe` pure Success
    it "Should return bad request in case category with such id does not exists" $ do
      let res = createPostHandler handle {getCategoryById = \_ -> pure []} user createPostRequest
      res `shouldBe` pure CategoryNotExist
    it "Should return bad request in case user not an author" $ do
      let res = createPostHandler handle user {Types.Entities.User.isAuthor = False} createPostRequest
      res `shouldBe` pure NotAuthor

