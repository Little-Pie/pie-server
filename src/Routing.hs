{-# LANGUAGE OverloadedStrings #-}

module Routing where

import qualified DbQuery.User as DBU
import qualified DbQuery.Post as DBP
import qualified DbQuery.Category as DBC
import qualified DbQuery.Image as DBI
import Helpers
import qualified Types.API.PostWithImages as API
import Types.Entities.Post
import Types.Entities.User
import qualified Types.Entities.Image as I
import qualified Types.Entities.GetPosts as GP
import Endpoints.CreateCategory
import Endpoints.EditPost
import Endpoints.EditCategory
import Endpoints.CreateUser
import Endpoints.CreatePost
import Endpoints.GetImageById
import Control.Monad
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Network.Wai (requestHeaders,Application,lazyRequestBody,rawPathInfo,rawQueryString,requestMethod,queryString)
import Network.HTTP.Types (methodGet,methodPost,Header,RequestHeaders)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Data.Aeson.Encode.Pretty (encodePretty)

application :: Connection -> Config -> Application
application conn config req respond
  | requestMethod req /= methodPost && requestMethod req /= methodGet = respond $ responseBadRequest "Use method GET or POST"
  | requestMethod req == methodPost = do
    body <- bodyIO
    case path of
      "" -> respond $
              if query' /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi POST!"
      "createUser" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUserId ->
          withParsedRequest body (createUser conn authorizedUserId)
        respond response
      "createPost" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUserId ->
          withParsedRequest body (createPost conn authorizedUserId)
        respond response
      "editPost" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUserId ->
          withParsedRequest body (editPost conn authorizedUserId)
        respond response
      "createCategory" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUserId ->
          withParsedRequest body (createCategory conn authorizedUserId)
        respond response
      "editCategory" -> do
        response <- withAuthorization conn mbBase64LoginAndPassword $ \authorizedUserId ->
          withParsedRequest body (editCategory conn authorizedUserId)
        respond response
      _ -> respond $ responseNotFound ""
  | path == "" = respond $
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
    let (mbLimit, mbOffset) = (join $ readMaybe . BS.unpack <$> lookup' "limit" queryItems :: Maybe Int, join $ readMaybe . BS.unpack <$> lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit config
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe (offset config) mbOffset
    categories <- DBC.showCategories conn limit' offset'
    respond $ responseOk $ encodePretty categories
  | path == "users" = do
    let (mbLimit, mbOffset) = (join $ readMaybe . BS.unpack <$> lookup' "limit" queryItems :: Maybe Int, join $ readMaybe . BS.unpack <$> lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit config
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe (offset config) mbOffset
    users <- DBU.showUsers conn limit' offset'
    respond $ responseOk $ encodePretty users
  | path == "posts" = do
    let queryFilters = getQueryFilters queryItems
    let mbQuerySortBy = lookup' "sortBy" queryItems
    let (mbLimit, mbOffset) = (join $ readMaybe . BS.unpack <$> lookup' "limit" queryItems :: Maybe Int, join $ readMaybe . BS.unpack <$> lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit config
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe (offset config) mbOffset
    posts <- DBP.showPosts conn limit' offset' queryFilters mbQuerySortBy
    images <- DBI.getImagesByPostIds conn (map GP.postId posts)
    let postsWithImages = mkPostsWithImages posts images
    respond $ responseOk $ encodePretty postsWithImages
  | otherwise = respond $ responseNotFound ""

  where queryItems = queryString req
        query' = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req
        headers = requestHeaders req
        mbBase64LoginAndPassword = getBase64LoginAndPassword headers

        getBase64LoginAndPassword :: RequestHeaders -> Maybe BS.ByteString
        getBase64LoginAndPassword headers = case headers of
          [] -> Nothing
          ((headerName,bStr):xs) -> if headerName == "Authorization"
            then Just bStr
            else getBase64LoginAndPassword xs

        mkPostsWithImages :: [GP.GetPosts] -> [I.Image] -> [API.PostWithImages]
        mkPostsWithImages [] _ = []
        mkPostsWithImages (post:posts) images = API.PostWithImages
          (GP.postId post)
          (GP.title post)
          (GP.text post)
          (GP.categoryId post)
          (GP.createdAt post)
          (GP.authorId post)
          (GP.isPublished post)
          (GP.authorName post)
          (GP.categoryName post)
          ((map (("http://localhost:4000/getImageById?id=" <>) . show . I.imageId) $ filter (\image -> I.postId image == GP.postId post) images)) : mkPostsWithImages posts images