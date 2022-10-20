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
application Environment {..} req respond
  | requestMethod req /= methodPost && requestMethod req /= methodGet = respond $ responseBadRequest "Use method GET or POST"
  | requestMethod req == methodPost = do
    body <- bodyIO
    case path of
      "" ->
        respond $
          if query' /= ""
            then responseBadRequest "No query parameters needed!"
            else responseOk "Hi POST!"
      "createUser" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createUser conn authorizedUser)
        respond response
      "createPost" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createPost conn authorizedUser)
        respond response
      "editPost" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (editPost conn authorizedUser)
        respond response
      "createCategory" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createCategory conn authorizedUser)
        respond response
      "editCategory" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (editCategory conn authorizedUser)
        respond response
      _ -> respond $ responseNotFound ""
  | path == "" =
    respond $
      if query' /= ""
        then responseBadRequest "No query parameters needed!"
        else responseOk "Hi GET!"
  | path == "getImageById" = case lookup' "id" queryItems of
    Nothing -> respond $ responseBadRequest "Enter image id"
    Just imageId -> case readMaybe (BS.unpack imageId) :: Maybe Int of
      Nothing -> respond $ responseBadRequest "Image id should be a number"
      Just imageId' -> do
        response <- getImageById conn imageId'
        respond response
  | path == "categories" = do
    let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe offset mbOffset
    categories <- DBC.showCategories conn limit' offset'
    respond $ responseOk $ encodePretty categories
  | path == "users" = do
    let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe offset mbOffset
    users <- DBU.showUsers conn limit' offset'
    respond $ responseOk $ encodePretty users
  | path == "posts" = do
    let queryFilters = getQueryFilters queryItems
    let mbQuerySortBy = lookup' "sortBy" queryItems
    let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe offset mbOffset
    posts <- DBP.showPosts conn limit' offset' queryFilters mbQuerySortBy
    images <- DBI.getImagesByPostIds conn (map GP.postId posts)
    let postsWithImages = mkPostsWithImages posts images
    respond $ responseOk $ encodePretty postsWithImages
  | otherwise = respond $ responseNotFound ""
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
