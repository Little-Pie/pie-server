{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoints.Posts where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified DbQuery.Image as DBI
import qualified DbQuery.Post as DBP
import Helpers (getQueryFilters, lookup', responseOk)
import Network.Wai (Response)
import Text.Read (readMaybe)
import qualified Types.API.PostWithImages as API
import qualified Types.Entities.GetPosts as GP
import qualified Types.Entities.Image as Image

getPosts :: [(BS.ByteString, Maybe BS.ByteString)] -> App Response
getPosts queryItems = do
  Environment {..} <- ask
  let queryFilters = getQueryFilters queryItems
  let mbQuerySortBy = lookup' "sortBy" queryItems
  let mbSearch = lookup' "search" queryItems
  let (mbLimit, mbOffset) =
        ( (readMaybe . BS.unpack) =<< lookup' "limit" queryItems,
          (readMaybe . BS.unpack) =<< lookup' "offset" queryItems
        )
  let cfgLimit = limit
  let limit' =
        if cfgLimit < fromMaybe cfgLimit mbLimit
          then cfgLimit
          else fromMaybe cfgLimit mbLimit
  let offset' = fromMaybe offset mbOffset
  posts <-
    liftIO $
      DBP.showPosts
        conn
        limit'
        offset'
        queryFilters
        mbQuerySortBy
        mbSearch
  images <-
    liftIO $
      DBI.getImagesByPostIds
        conn
        (map GP.postId posts)
  let postsWithImages = mkPostsWithImages posts images
  responseOk $ encodePretty postsWithImages

mkPostsWithImages :: [GP.GetPosts] -> [Image.Image] -> [API.PostWithImages]
mkPostsWithImages [] _ = []
mkPostsWithImages (post : posts) images =
  API.PostWithImages
    (GP.postId post)
    (GP.title post)
    (GP.text post)
    (GP.categoryId post)
    (GP.createdAt post)
    (GP.authorId post)
    (GP.isPublished post)
    (GP.authorName post)
    (GP.categoryName post)
    ( map (("http://localhost:4000/getImageById?id=" <>) . show . Image.imageId) $
        filter (\image -> Image.postId image == GP.postId post) images
    ) :
  mkPostsWithImages posts images
