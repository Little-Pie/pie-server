{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), createUserHandler)
import Types.Entities.User (User (..))
import Types.API.CreateUser (CreateUserRequest (..))
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, it, shouldBe, SpecWith)

handle :: Handle Identity
handle = Handle
  { insertNewUser = \_ _ _ _ _ -> pure (),
    getUserByLogin = \_ -> pure []
  }

user :: User
user = User 1 "name" "login" "password" undefined True True

createUserRequest :: CreateUserRequest
createUserRequest = CreateUserRequest "name1" "login1" "password1" False False

createUserTest :: SpecWith ()
createUserTest =
  describe "User creation tests" $ do
    it "Should successfuly create user when requested by admin" $ do
      let res = createUserHandler handle user createUserRequest
      res `shouldBe` pure Success
    it "Should return bad request in case user with such login already exists" $ do
      let res = createUserHandler handle {getUserByLogin = \_ -> pure [user]} user createUserRequest
      res `shouldBe` pure LoginIsTaken
    it "Should return not found in case user not an admin" $ do
      let res = createUserHandler handle user {Types.Entities.User.isAdmin = False} createUserRequest
      res `shouldBe` pure NotFound
