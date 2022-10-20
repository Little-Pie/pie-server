{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import Data.Functor.Identity (Identity)
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), createUserHandler)
import Fixtures (userAdminAuthor, userNotAdminAuthor)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Types.API.CreateUser (CreateUserRequest (..))
import Types.Entities.User (User (..))

handle :: Handle Identity
handle =
  Handle
    { insertNewUser = \_ _ _ _ _ -> pure (),
      getUserByLogin = \_ -> pure []
    }

createUserRequest :: CreateUserRequest
createUserRequest = CreateUserRequest "name1" "login1" "password1" False False

createUserTest :: SpecWith ()
createUserTest =
  describe "User creation tests" $ do
    it "Should successfuly create user when requested by admin" $ do
      let res = createUserHandler handle userAdminAuthor createUserRequest
      res `shouldBe` pure Success
    it "Should return bad request in case user with such login already exists" $ do
      let res = createUserHandler handle {getUserByLogin = \_ -> pure [userAdminAuthor]} userAdminAuthor createUserRequest
      res `shouldBe` pure LoginIsTaken
    it "Should return not found in case user not an admin" $ do
      let res = createUserHandler handle userNotAdminAuthor createUserRequest
      res `shouldBe` pure NotFound
