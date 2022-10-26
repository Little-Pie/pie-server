{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Routing where

import Config (App, Environment (..))
import Control.Monad.Reader (ask, liftIO)
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
import Network.Wai (Application, Request, Response, ResponseReceived, lazyRequestBody, queryString, rawPathInfo, rawQueryString, requestHeaders, requestMethod)
import Text.Read (readMaybe)
import qualified Types.API.PostWithImages as API
import qualified Types.Entities.GetPosts as GP
import qualified Types.Entities.Image as Image

application :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
application req respond
  | requestMethod req == methodPost = do
    body <- liftIO bodyIO
    case path of
      "createUser" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createUser authorizedUser)
        liftIO $ respond response
      "createPost" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createPost authorizedUser)
        liftIO $ respond response
      "editPost" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (editPost authorizedUser)
        liftIO $ respond response
      "createCategory" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createCategory authorizedUser)
        liftIO $ respond response
      "editCategory" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (editCategory authorizedUser)
        liftIO $ respond response
      _ -> do
        response <- responseNotFound ""
        liftIO $ respond response
  | requestMethod req == methodGet = do
    Environment {..} <- ask
    case path of
      "getImageById" -> case lookup' "id" queryItems of
        Nothing -> do
          response <- responseBadRequest "Enter image id"
          liftIO $ respond response
        Just imageId -> case readMaybe (BS.unpack imageId) :: Maybe Int of
          Nothing -> do
            response <- responseBadRequest "Image id should be a number"
            liftIO $ respond response
          Just imageId' -> do
            response <- getImageById imageId'
            liftIO $ respond response
      "categories" -> do
        let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
        let cfgLimit = limit
        let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
        let offset' = fromMaybe offset mbOffset
        categories <- liftIO $ DBC.showCategories conn limit' offset'
        response <- responseOk $ encodePretty categories
        liftIO $ respond response
      "users" -> do
        let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
        let cfgLimit = limit
        let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
        let offset' = fromMaybe offset mbOffset
        users <- liftIO $ DBU.showUsers conn limit' offset'
        response <- responseOk $ encodePretty users
        liftIO $ respond response
      "posts" -> do
        let queryFilters = getQueryFilters queryItems
        let mbQuerySortBy = lookup' "sortBy" queryItems
        let mbSearch = lookup' "search" queryItems
        let (mbLimit, mbOffset) = ((readMaybe . BS.unpack) =<< lookup' "limit" queryItems :: Maybe Int, (readMaybe . BS.unpack) =<< lookup' "offset" queryItems :: Maybe Int)
        let cfgLimit = limit
        let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
        let offset' = fromMaybe offset mbOffset
        posts <- liftIO $ DBP.showPosts conn limit' offset' queryFilters mbQuerySortBy mbSearch
        images <- liftIO $ DBI.getImagesByPostIds conn (map GP.postId posts)
        let postsWithImages = mkPostsWithImages posts images
        response <- responseOk $ encodePretty postsWithImages
        liftIO $ respond response
      _ -> do
        response <- responseNotFound ""
        liftIO $ respond response
  | otherwise = do
    response <- responseNotFound ""
    liftIO $ respond response
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
