{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), createCategoryHandler)
import Types.Entities.User (User (..))
import Types.Entities.Category (Category (..))
import Types.API.CreateCategory (CreateCategoryRequest (..))
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, it, shouldBe, SpecWith)

handle :: Handle Identity
handle = Handle
  { getGeneralCategoryByName = \_ -> pure [],
    insertNewGeneralCategory = \_ -> pure (),
    getCategoryByNameAndParent = \_ _ -> pure [],
    insertNewCategory = \_ _ -> pure ()
  }

user :: User
user = User 1 "name" "login" "password" undefined True True

createCategoryRequest :: CreateCategoryRequest
createCategoryRequest = CreateCategoryRequest "name" (Just 1)

category :: Category
category = Category 1 "name" (Just 1)

createCategoryTest :: SpecWith ()
createCategoryTest =
  describe "Category creation tests" $ do
    it "Should successfuly create general category when requested by admin" $ do
      let res = createCategoryHandler handle user createCategoryRequest {Types.API.CreateCategory.parentCategoryId = Nothing}
      res `shouldBe` pure Success
    it "Should successfuly create category when requested by admin" $ do
      let res = createCategoryHandler handle user createCategoryRequest
      res `shouldBe` pure Success
    it "Should return bad request in case general category with such name already exists" $ do
      let res = createCategoryHandler handle {getGeneralCategoryByName = \_ -> pure [category {Types.Entities.Category.parentCategoryId = Nothing}]} user createCategoryRequest {Types.API.CreateCategory.parentCategoryId = Nothing}
      res `shouldBe` pure NameIsTaken
    it "Should return bad request in case category with such name already exists" $ do
      let res = createCategoryHandler handle {getCategoryByNameAndParent = \_ _ -> pure [category]} user createCategoryRequest
      res `shouldBe` pure NameIsTaken
    it "Should return not found in case user not an admin" $ do
      let res = createCategoryHandler handle user {Types.Entities.User.isAdmin = False} createCategoryRequest
      res `shouldBe` pure NotFound