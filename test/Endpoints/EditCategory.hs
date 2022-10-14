{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditCategory where

import Endpoints.Handlers.EditCategory (EditCategoryResult (..), Handle (..), editCategoryHandler)
import Types.Entities.User (User (..))
import Types.Entities.Category as Category (Category (..))
import Types.API.EditCategory as EditCategory (EditCategoryRequest (..))
import Data.Functor.Identity (Identity)
import Test.Hspec (describe, it, shouldBe, SpecWith)

handle :: Handle Identity
handle = Handle
  { getGeneralCategoryByName = \_ -> pure [],
    getCategoryByNameAndParent = \_ _ -> pure [],
    editCategory = \_ _ _ -> pure (),
    getCategoryById = \_ -> pure [category],
    getCategoryByParentId = \_ -> pure []
  }

user :: User
user = User 1 "name" "login" "password" undefined True True

editCategoryRequest :: EditCategoryRequest
editCategoryRequest = EditCategoryRequest 1 (Just "name") (Just 3)

category :: Category
category = Category 2 "name" (Just 1)

generalCategory :: Category
generalCategory = Category 1 "name" Nothing

editCategoryTest :: SpecWith ()
editCategoryTest =
  describe "Category creation tests" $ do
    it "Should successfuly edit category when requested by admin" $ do
      let res = editCategoryHandler handle user editCategoryRequest {EditCategory.parentCategoryId = Just 5}
      res `shouldBe` pure Success
    it "Should successfuly edit general category when requested by admin" $ do
      let res = editCategoryHandler handle {getCategoryById = \_ -> pure [generalCategory]} user editCategoryRequest {EditCategory.parentCategoryId = Nothing}
      res `shouldBe` pure Success
    it "Should return bad request in case category with such id does not exist" $ do
      let res = editCategoryHandler handle {getCategoryById = \_ -> pure []} user editCategoryRequest
      res `shouldBe` pure CategoryNotExist
    it "Should return bad request in case general category with such name already exists" $ do
      let res = editCategoryHandler handle {getCategoryById = \_ -> pure [generalCategory], getGeneralCategoryByName = \_ -> pure [category]} user editCategoryRequest {EditCategory.parentCategoryId = Nothing}
      res `shouldBe` pure NameIsTaken
    it "Should return bad request in case parent category with such id does not exist" $ do
      let handleCase = handle
            { getCategoryById = \id -> if id == 1 then pure [generalCategory] else pure []
            } 
      let res = editCategoryHandler handleCase user editCategoryRequest
      res `shouldBe` pure ParentCategoryNotExist
    it "Should return bad request in case category with such name already exists" $ do
      let res = editCategoryHandler handle {getCategoryByNameAndParent = \_ _ -> pure [category]} user editCategoryRequest
      res `shouldBe` pure NameIsTaken
    it "Should return not found in case user not an admin" $ do
      let res = editCategoryHandler handle user {isAdmin = False} editCategoryRequest
      res `shouldBe` pure NotFound
    it "Should return bad request in case category id equal parent category id" $ do
      let res = editCategoryHandler handle user editCategoryRequest {EditCategory.parentCategoryId = Just 1}
      res `shouldBe` pure IllegalParentCategoryId
    it "Should return bad request in case parent category id is invalid" $ do
      let res = editCategoryHandler handle {getCategoryByParentId = \[1] -> pure [category {Category.categoryId = 1}]} user editCategoryRequest {EditCategory.parentCategoryId = Just 1}
      res `shouldBe` pure IllegalParentCategoryId
    