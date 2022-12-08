module Types.Entities.Image where

import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Image = Image
  { imageId :: Int,
    postId :: Int,
    base64Image :: String,
    contentType :: String
  }
  deriving (Eq, Show, Generic, ToJSON, FromRow)
