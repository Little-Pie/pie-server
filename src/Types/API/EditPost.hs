module Types.API.EditPost where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data EditPostRequest = EditPostRequest
  { postId :: Int,
    title :: Maybe String,
    text :: Maybe String,
    categoryId :: Maybe Int,
    isPublished :: Maybe Bool,
    base64Images :: Maybe [String],
    contentTypes :: Maybe [String]
  }
  deriving (Generic, FromJSON)
