module Endpoints.GetImageById where

import Data.Functor.Identity (Identity (..))
import Endpoints.Handlers.GetImageById
  ( GetImageByIdResult (..),
    Handle (..),
    getImageByIdHandler,
  )
import Fixtures (image)
import Test.Hspec
  ( SpecWith,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Types.Entities.Image (Image (..))

handle :: Handle Identity
handle =
  Handle
    { getImageById = \_ -> pure [image]
    }

satisfySuccess :: Identity GetImageByIdResult -> Bool
satisfySuccess res =
  case res of
    Identity (Success _ _) -> True
    _ -> False

getImageByIdTest :: SpecWith ()
getImageByIdTest =
  describe "Getting image by id tests" $ do
    it "Should successfuly return image" $ do
      let res =
            getImageByIdHandler
              handle
              1
      res `shouldSatisfy` satisfySuccess
    it "Should return bad request in case image with such id does not exist" $ do
      let res =
            getImageByIdHandler
              handle {getImageById = \_ -> pure []}
              1 ::
              Identity GetImageByIdResult
      res `shouldBe` pure ImageNotExist
    it "Should return bad request in case decoding wasn't successful" $ do
      let res =
            getImageByIdHandler
              handle {getImageById = \_ -> pure [image {base64Image = "a"}]}
              1 ::
              Identity GetImageByIdResult
      res `shouldBe` pure DecodeError
