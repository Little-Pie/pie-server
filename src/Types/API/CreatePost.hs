module Types.API.CreatePost where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data CreatePostRequest = CreatePostRequest
  { title :: String,
    text :: String,
    categoryId :: Int,
    isPublished :: Bool,
    base64Images :: [String],
    contentTypes :: [String]
  }
  deriving (Generic, FromJSON)
