module Types.API.EditCategory where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data EditCategoryRequest = EditCategoryRequest
  { categoryId :: Int,
    name :: Maybe String,
    parentCategoryId :: Maybe Int
  }
  deriving (Generic, FromJSON)
