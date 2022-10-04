{-# LANGUAGE OverloadedStrings #-}

module Routing where

import qualified DbQuery.User as DBU
import qualified DbQuery.Post as DBP
import qualified DbQuery.Category as DBC
import Helpers
import Types.Entities.Post
import Types.Entities.User
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
        (str, mbId) <- authorize conn mbBase64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just authorizedUserId -> do
            response <- createUser conn body authorizedUserId
            respond response
      "createPost" -> do
        (str, mbId) <- authorize conn mbBase64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just authorizedUserId -> do
            response <- createPost conn body authorizedUserId
            respond response 
      "editPost" -> do
        (str, mbId) <- authorize conn mbBase64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just authorizedUserId -> do
            response <- editPost conn body authorizedUserId
            respond response
      "createCategory" -> do
        (str, mbId) <- authorize conn mbBase64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just authorizedUserId -> do
            response <- createCategory conn body authorizedUserId
            respond response
      "editCategory" -> do
        (str, mbId) <- authorize conn mbBase64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just authorizedUserId -> do
            response <- editCategory conn body authorizedUserId
            respond response
      "getImageById" -> do
        response <- getImageById conn body
        respond response
      _ -> respond $ responseNotFound ""
  | path == "" = respond $
              if query' /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi GET!"
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
    respond $ responseOk $ encodePretty posts
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