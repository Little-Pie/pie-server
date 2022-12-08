module Types.API.PostWithImages where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data PostWithImages = PostWithImages
  { postId :: Int,
    title :: String,
    text :: String,
    categoryId :: Int,
    createdAt :: UTCTime,
    authorId :: Int,
    isPublished :: Bool,
    authorName :: String,
    categoryName :: String,
    imagesURL :: [String]
  }
  deriving (Generic, ToJSON)
