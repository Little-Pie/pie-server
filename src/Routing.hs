{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Routing where

import Config (Environment (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (Connection)
import qualified DbQuery.Category as DBC
import qualified DbQuery.Image as DBI
import qualified DbQuery.Post as DBP
import qualified DbQuery.User as DBU
import Endpoints (createCategory, createPost, createUser, editCategory, editPost, getImageById)
import Helpers (getQueryFilters, lookup', responseBadRequest, responseNotFound, responseOk, withAuthorization, withParsedRequest)
import Network.HTTP.Types (Header, RequestHeaders, methodGet, methodPost)
import Network.Wai (Application, lazyRequestBody, queryString, rawPathInfo, rawQueryString, requestHeaders, requestMethod)
import Text.Read (readMaybe)
import qualified Types.API.PostWithImages as API
import qualified Types.Entities.GetPosts as GP
import qualified Types.Entities.Image as Image

application :: Environment -> Application
application env@Environment {..} req respond
  | requestMethod req == methodPost = do
    body <- bodyIO
    case path of
      "createUser" -> do
        response <- withAuthorization env mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest env body (createUser env authorizedUser)
        respond response
      "createPost" -> do
        response <- withAuthorization env mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest env body (createPost env authorizedUser)
        respond response
      "editPost" -> do
        response <- withAuthorization env mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest env body (editPost env authorizedUser)
        respond response
      "createCategory" -> do
        response <- withAuthorization env mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest env body (createCategory env authorizedUser)
        respond response
      "editCategory" -> do
        response <- withAuthorization env mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest env body (editCategory env authorizedUser)
        respond response
      _ -> do
        response <- responseNotFound env ""
        respond response
  | requestMethod req == methodGet = do
    case path of
      "getImageById" -> case lookup' "id" queryItems of
        Nothing -> do
          response <- responseBadRequest env "Enter image id"
          respond response
        Just imageId -> case readMaybe (BS.unpack imageId) :: Maybe Int of
          Nothing -> do
            response <- responseBadRequest env "Image id should be a number"
            respond response
          Just imageId' -> do
            response <- getImageById env imageId'
            respond response
      "categories" -> do
        let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
        let cfgLimit = limit
        let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
        let offset' = fromMaybe offset mbOffset
        categories <- DBC.showCategories conn limit' offset'
        response <- responseOk env $ encodePretty categories
        respond response
      "users" -> do
        let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
        let cfgLimit = limit
        let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
        let offset' = fromMaybe offset mbOffset
        users <- DBU.showUsers conn limit' offset'
        response <- responseOk env $ encodePretty users
        respond response
      "posts" -> do
        let queryFilters = getQueryFilters queryItems
        let mbQuerySortBy = lookup' "sortBy" queryItems
        let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
        let cfgLimit = limit
        let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
        let offset' = fromMaybe offset mbOffset
        posts <- DBP.showPosts conn limit' offset' queryFilters mbQuerySortBy
        images <- DBI.getImagesByPostIds conn (map GP.postId posts)
        let postsWithImages = mkPostsWithImages posts images
        response <- responseOk env $ encodePretty postsWithImages
        respond response
      _ -> do
        response <- responseNotFound env ""
        respond response
  | otherwise = do
    response <- responseNotFound env ""
    respond response
  where
    queryItems = queryString req
    query' = rawQueryString req
    path = BS.tail $ rawPathInfo req
    bodyIO = lazyRequestBody req
    headers = requestHeaders req
    mbBase64LoginAndPassword = getBase64LoginAndPassword headers

    getBase64LoginAndPassword :: RequestHeaders -> Maybe BS.ByteString
    getBase64LoginAndPassword headers = case headers of
      [] -> Nothing
      ((headerName, bStr) : xs) ->
        if headerName == "Authorization"
          then Just bStr
          else getBase64LoginAndPassword xs

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
        (map (("http://localhost:4000/getImageById?id=" <>) . show . Image.imageId) $ filter (\image -> Image.postId image == GP.postId post) images) :
      mkPostsWithImages posts images
