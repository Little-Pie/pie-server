module Endpoints.EditCategory where

import Data.Functor.Identity (Identity)
import Endpoints.Handlers.EditCategory
  ( EditCategoryResult (..),
    Handle (..),
    editCategoryHandler,
  )
import Fixtures
  ( category,
    categoryRoot,
    userAdminAuthor,
    userNotAdminAuthor,
  )
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
  )
import Types.API.EditCategory as EditCategory (EditCategoryRequest (..))
import Types.Entities.Category as Category (Category (..))
import Types.Entities.User (User (..))

handle :: Handle Identity
handle =
  Handle
    { getGeneralCategoryByName = \_ -> pure [],
      getCategoryByNameAndParent = \_ _ -> pure [],
      editCategory = \_ _ _ -> pure (),
      getCategoryById = \_ -> pure [category],
      getCategoryByParentId = \_ -> pure []
    }

editCategoryRequest :: EditCategoryRequest
editCategoryRequest = EditCategoryRequest 1 (Just "name") (Just 3)

editCategoryTest :: SpecWith ()
editCategoryTest =
  describe "Category editing tests" $ do
    it "Should successfuly edit category when requested by admin" $ do
      let res =
            editCategoryHandler
              handle
              userAdminAuthor
              editCategoryRequest {EditCategory.parentCategoryId = Just 5}
      res `shouldBe` pure Success
    it "Should successfuly edit general category when requested by admin" $ do
      let res =
            editCategoryHandler
              handle {getCategoryById = \_ -> pure [categoryRoot]}
              userAdminAuthor
              editCategoryRequest {EditCategory.parentCategoryId = Nothing}
      res `shouldBe` pure Success
    it "Should return bad request in case category with such id does not exist" $ do
      let res =
            editCategoryHandler
              handle {getCategoryById = \_ -> pure []}
              userAdminAuthor
              editCategoryRequest
      res `shouldBe` pure CategoryNotExist
    it "Should return bad request in case general category with such name already exists" $ do
      let res =
            editCategoryHandler
              handle
                { getCategoryById = \_ -> pure [categoryRoot],
                  getGeneralCategoryByName = \_ -> pure [category]
                }
              userAdminAuthor
              editCategoryRequest {EditCategory.parentCategoryId = Nothing}
      res `shouldBe` pure NameIsTaken
    it "Should return bad request in case parent category with such id does not exist" $ do
      let handleCase =
            handle
              { getCategoryById = \id -> if id == 1 then pure [categoryRoot] else pure []
              }
      let res =
            editCategoryHandler
              handleCase
              userAdminAuthor
              editCategoryRequest
      res `shouldBe` pure ParentCategoryNotExist
    it "Should return bad request in case category with such name already exists" $ do
      let res =
            editCategoryHandler
              handle {getCategoryByNameAndParent = \_ _ -> pure [category]}
              userAdminAuthor
              editCategoryRequest
      res `shouldBe` pure NameIsTaken
    it "Should return not found in case user not an admin" $ do
      let res =
            editCategoryHandler
              handle
              userNotAdminAuthor
              editCategoryRequest
      res `shouldBe` pure NotFound
    it "Should return bad request in case category id equal parent category id" $ do
      let res =
            editCategoryHandler
              handle
              userAdminAuthor
              editCategoryRequest {EditCategory.parentCategoryId = Just 1}
      res `shouldBe` pure IllegalParentCategoryId
    it "Should return bad request in case parent category id is invalid" $ do
      let res =
            editCategoryHandler
              handle
                { getCategoryByParentId = \[1] ->
                    pure
                      [category {Category.categoryId = 1}]
                }
              userAdminAuthor
              editCategoryRequest {EditCategory.parentCategoryId = Just 1}
      res `shouldBe` pure IllegalParentCategoryId
