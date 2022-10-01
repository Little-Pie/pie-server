{-# LANGUAGE OverloadedStrings #-}

module Routing where

import qualified DbQuery.User as DBU
import qualified DbQuery.Post as DBP
import qualified DbQuery.Category as DBC
import Helpers
import Types.Entities.Post
import Types.Entities.User
import Endpoints.GetPostById
import Endpoints.GetUserById
import Endpoints.GetCategoryById
import Endpoints.CreateCategory
import Endpoints.DeleteUser
import Endpoints.DeletePost
import Endpoints.EditPost
import Endpoints.RemoveAdmin
import Endpoints.RemoveAuthor
import Endpoints.MakeAdmin
import Endpoints.MakeAuthor
import Endpoints.CreateUser
import Endpoints.EditUser
import Endpoints.CreatePost
import Endpoints.PublishPost
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
        response <- createUser conn body
        respond response
      "userById" -> do
        response <- getUserById conn body
        respond response
      "postById" -> do
        response <- getPostById conn body
        respond response
      "categoryById" -> do
        response <- getCategoryById conn body
        respond response
      "editUser" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
                case lookup' "id" queryItems of
                  Just userId -> do
                    admin <- DBU.getUserById conn id
                    case admin of
                      [] -> respond $ responseInternalError "Something went wrong: empty list"
                      _ -> case isAdmin $ head admin of
                        True -> do
                          case readMaybe (BS.unpack userId) :: Maybe Int of
                            Nothing -> respond $ responseBadRequest "User id should be a number"
                            Just userId' -> do
                              response <- editUser conn body userId'
                              respond response
                        False -> respond $ responseBadRequest "Only admin can edit other users"
                  Nothing -> do
                    response <- editUser conn body id
                    respond response
      "createPost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            author <- DBU.getUserById conn id
            case isAuthor $ head author of
              False -> respond $ responseNotFound "You can not post news"
              True -> do
                putStrLn $ LBSC.unpack body
                response <- createPost conn body id
                LBSC.putStrLn response
                respond $ responseOk response 
      "editPost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            case lookup' "id" queryItems of
              Nothing -> respond $ responseBadRequest "Enter post id"
              Just postId -> do
                admin <- DBU.getUserById conn id
                case admin of
                  [] -> respond $ responseInternalError "Something went wrong: empty list"
                  _ -> case isAdmin $ head admin of
                    True -> case readMaybe (BS.unpack postId) :: Maybe Int of
                      Nothing -> respond $ responseBadRequest "Post id should be a number"
                      Just userId' -> do
                        response <- editPost conn body userId'
                        LBSC.putStrLn str
                        putStrLn $ LBSC.unpack body
                        LBSC.putStrLn response
                        respond $ responseOk $ str `mappend` "\n" `mappend` response
                    False -> case lookup' "id" queryItems of
                      Nothing -> respond $ responseBadRequest "Enter post id"
                      Just postId -> case readMaybe (BS.unpack postId) :: Maybe Int of
                        Nothing -> respond $ responseBadRequest "Post id should be a number"
                        Just postId' -> do
                          post <- DBP.getPostById conn postId'
                          case post of
                            [] -> respond $ responseInternalError "Something went wrong: empty list"
                            [x] -> case id == authorId x of
                              False -> respond $ responseNotFound "You're not able to edit this post"
                              True -> do
                                response <- editPost conn body postId'
                                LBSC.putStrLn response
                                respond $ responseOk $ str `mappend` "\n" `mappend` response
      "makeAuthor" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- DBU.getUserById conn id
            case isAdmin $ head admin of
              False -> respond $ responseNotFound "You can not make authors"
              True -> do
                if query' == ""
                then respond $ responseBadRequest "Enter user id"
                else do
                  case lookup' "id" queryItems of
                    Nothing -> respond $ responseBadRequest "Enter user id"
                    Just userId -> do
                      case readMaybe (BS.unpack userId) :: Maybe Int of
                        Nothing -> respond $ responseBadRequest "User id should be a number"
                        Just userId' -> do
                          response <- makeAuthor conn body
                          respond response
      "removeAuthor" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- DBU.getUserById conn id
            case isAdmin $ head admin of
              False -> respond $ responseNotFound "You can not remove authors"
              True -> do
                if query' == ""
                then respond $ responseBadRequest "Enter user id"
                else do
                  case lookup' "id" queryItems of
                    Nothing -> respond $ responseBadRequest "Enter user id"
                    Just userId -> do
                      case readMaybe (BS.unpack userId) :: Maybe Int of
                        Nothing -> respond $ responseBadRequest "User id should be a number"
                        Just userId' -> do
                          response <- removeAuthor conn body
                          respond response
      "makeAdmin" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- DBU.getUserById conn id
            case isAdmin $ head admin of
              False -> respond $ responseNotFound "You can not make admins"
              True -> do
                if query' == ""
                then respond $ responseBadRequest "Enter user id"
                else do
                  case lookup' "id" queryItems of
                    Nothing -> respond $ responseBadRequest "Enter user id"
                    Just userId -> do
                      case readMaybe (BS.unpack userId) :: Maybe Int of
                        Nothing -> respond $ responseBadRequest "User id should be a number"
                        Just userId' -> do
                          response <- makeAdmin conn body
                          respond response
      "removeAdmin" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- DBU.getUserById conn id
            case isAdmin $ head admin of
              False -> respond $ responseNotFound "You can not remove admins"
              True -> do
                if query' == ""
                then respond $ responseBadRequest "Enter user id"
                else do
                  case lookup' "id" queryItems of
                    Nothing -> respond $ responseBadRequest "Enter user id"
                    Just userId -> do
                      case readMaybe (BS.unpack userId) :: Maybe Int of
                        Nothing -> respond $ responseBadRequest "User id should be a number"
                        Just userId' -> do
                          response <- removeAdmin conn body
                          respond response
      "publishPost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            response <- publishPost conn body userId
            LBSC.putStrLn response
            respond $ responseOk response
      "deletePost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            response <- deletePost conn body userId
            LBSC.putStrLn response
            respond $ responseOk $ str `mappend` "\n" `mappend` response
      "deleteUser" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            response <- deleteUser conn body userId
            respond response
      "createCategory" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            response <- createCategory conn body userId
            LBSC.putStrLn response
            respond $ responseOk $ str `mappend` "\n" `mappend` response
      _ -> respond $ responseNotFound "Unknown method called"
  | path == "" = respond $
              if query' /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi GET!"
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
  | otherwise = respond $ responseNotFound "Unknown method called"

  where queryItems = queryString req
        query' = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req
        headers = requestHeaders req
        base64LoginAndPassword = snd $ head $ filter (\x -> fst x == "Authorization") headers