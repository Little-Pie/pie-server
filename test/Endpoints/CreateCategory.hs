{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateCategory where

import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), createCategoryHandler)
import Types.Entities.User (User (..))
import Types.Entities.Category (Category (..))
import Types.API.CreateCategory (CreateCategoryRequest (..))
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Fixtures (userAdminAuthor,userNotAdminAuthor, category)

handle :: Handle Identity
handle = Handle
  { getGeneralCategoryByName = \_ -> pure [],
    insertNewGeneralCategory = \_ -> pure (),
    getCategoryByNameAndParent = \_ _ -> pure [],
    insertNewCategory = \_ _ -> pure (),
    getCategoryById = \_ -> pure [category]
  }

createCategoryRequest :: CreateCategoryRequest
createCategoryRequest = CreateCategoryRequest "name" (Just 1)

createCategoryTest :: SpecWith ()
createCategoryTest =
  describe "Category creation tests" $ do
    it "Should successfuly create general category when requested by admin" $ do
      let res = createCategoryHandler handle userAdminAuthor createCategoryRequest {Types.API.CreateCategory.parentCategoryId = Nothing}
      res `shouldBe` pure Success
    it "Should successfuly create category when requested by admin" $ do
      let res = createCategoryHandler handle userAdminAuthor createCategoryRequest
      res `shouldBe` pure Success
    it "Should return bad request in case general category with such name already exists" $ do
      let res = createCategoryHandler handle {getGeneralCategoryByName = \_ -> pure [category {Types.Entities.Category.parentCategoryId = Nothing}]} userAdminAuthor createCategoryRequest {Types.API.CreateCategory.parentCategoryId = Nothing}
      res `shouldBe` pure NameIsTaken
    it "Should return bad request in case parent category with such id does not exist" $ do
      let res = createCategoryHandler handle {getCategoryById = \_ -> pure []} userAdminAuthor createCategoryRequest
      res `shouldBe` pure ParentCategoryNotExist
    it "Should return bad request in case category with such name already exists" $ do
      let res = createCategoryHandler handle {getCategoryByNameAndParent = \_ _ -> pure [category]} userAdminAuthor createCategoryRequest
      res `shouldBe` pure NameIsTaken
    it "Should return not found in case user not an admin" $ do
      let res = createCategoryHandler handle userNotAdminAuthor createCategoryRequest
      res `shouldBe` pure NotFound