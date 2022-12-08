module Types.Entities.Category where

import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Category = Category
  { categoryId :: Int,
    name :: String,
    parentCategoryId :: Maybe Int
  }
  deriving (Generic, ToJSON, FromRow)
