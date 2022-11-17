{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.API.CreateCategory where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data CreateCategoryRequest = CreateCategoryRequest
  { name :: String,
    parentCategoryId :: Maybe Int
  }
  deriving (FromJSON, Generic)
