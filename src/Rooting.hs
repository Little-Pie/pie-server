{-# LANGUAGE OverloadedStrings #-}

module Rooting where

import Helpers
import Types.Entities.Post
import Types.Entities.User
import qualified Types.API.GetPosts as API
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
        putStrLn $ LBSC.unpack body
        --time <- getCurrentTime
        --let timeStamp = LBSC.pack $ take 19 $ show time
        answer <- createUser conn body
        LBSC.putStrLn answer
        respond $ responseOk answer
      "editUser" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
                case lookup' "id" queryItems of
                  Just userId -> do
                    admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
                    case admin of
                      [] -> respond $ responseInternalError "Something went wrong: empty list"
                      _ -> case isAdmin $ head admin of
                        True -> do
                          case readMaybe (BS.unpack userId) :: Maybe Int of
                            Nothing -> respond $ responseBadRequest "User id should be a number"
                            Just userId' -> do
                              answer <- editUser conn body userId'
                              LBSC.putStrLn str
                              putStrLn $ LBSC.unpack body
                              LBSC.putStrLn answer
                              respond $ responseOk $ str `mappend` "\n" `mappend` answer
                        False -> respond $ responseBadRequest "Only admin can edit other users"
                  Nothing -> do
                    LBSC.putStrLn str
                    putStrLn $ LBSC.unpack body
                    answer <- editUser conn body id
                    LBSC.putStrLn answer
                    respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "createPost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            author <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
            case isAuthor $ head author of
              False -> respond $ responseNotFound "You can not post news"
              True -> do
                putStrLn $ LBSC.unpack body
                answer <- createPost conn body id
                LBSC.putStrLn answer
                respond $ responseOk answer 
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
                admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
                case admin of
                  [] -> respond $ responseInternalError "Something went wrong: empty list"
                  _ -> case isAdmin $ head admin of
                    True -> case readMaybe (BS.unpack postId) :: Maybe Int of
                      Nothing -> respond $ responseBadRequest "Post id should be a number"
                      Just userId' -> do
                        answer <- editPost conn body userId'
                        LBSC.putStrLn str
                        putStrLn $ LBSC.unpack body
                        LBSC.putStrLn answer
                        respond $ responseOk $ str `mappend` "\n" `mappend` answer
                    False -> case lookup' "id" queryItems of
                      Nothing -> respond $ responseBadRequest "Enter post id"
                      Just postId -> case readMaybe (BS.unpack postId) :: Maybe Int of
                        Nothing -> respond $ responseBadRequest "Post id should be a number"
                        Just postId' -> do
                          post <- query conn "select * from posts where id=(?)" (Only postId') :: IO [Post]
                          case post of
                            [] -> respond $ responseInternalError "Something went wrong: empty list"
                            [x] -> case id == authorId x of
                              False -> respond $ responseNotFound "You're not able to edit this post"
                              True -> do
                                answer <- editPost conn body postId'
                                LBSC.putStrLn answer
                                respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "makeAuthor" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
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
                          answer <- makeAuthor conn body
                          LBSC.putStrLn answer
                          respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "removeAuthor" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
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
                          answer <- removeAuthor conn body
                          LBSC.putStrLn answer
                          respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "makeAdmin" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
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
                          answer <- makeAdmin conn body
                          LBSC.putStrLn answer
                          respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "removeAdmin" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
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
                          answer <- removeAdmin conn body
                          LBSC.putStrLn answer
                          respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "publishPost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            answer <- publishPost conn body userId
            LBSC.putStrLn answer
            respond $ responseOk answer
      "deletePost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            answer <- deletePost conn body userId
            LBSC.putStrLn answer
            respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "deleteUser" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            answer <- deleteUser conn body userId
            LBSC.putStrLn answer
            respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "createCategory" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just userId -> do
            answer <- createCategory conn body userId
            LBSC.putStrLn answer
            respond $ responseOk $ str `mappend` "\n" `mappend` answer
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
    users <- query conn "select * from users limit (?) offset (?)" (limit', offset') :: IO [User]
    respond $ responseOk $ encodePretty users
  | path == "posts" = do
    let sorts = ["category","created_at","title","author"]
    let initQuery = "SELECT posts.id,posts.title,posts.text,posts.category_id,posts.created_at,posts.author_id,posts.is_published,users.name,categories.name FROM posts JOIN users ON posts.author_id = users.id JOIN categories ON posts.category_id = categories.id WHERE is_published = true"
    let sortBy = fromMaybe ("") ((\n -> if n `elem` sorts then (if n == "category" then " order by categories.name " else (if n == "author" then " order by users.name " else " order by " <> Query n)) else "") <$> lookup' "sort_by" queryItems)
    let (mbLimit, mbOffset) = (join $ readMaybe . BS.unpack <$> lookup' "limit" queryItems :: Maybe Int, join $ readMaybe . BS.unpack <$> lookup' "offset" queryItems :: Maybe Int)
    let cfgLimit = limit config
    let limit' = if cfgLimit < fromMaybe cfgLimit mbLimit then cfgLimit else fromMaybe cfgLimit mbLimit
    let offset' = fromMaybe (offset config) mbOffset
    posts <- query conn (initQuery <> sortBy <> " limit (?) offset (?)") (limit', offset') :: IO [API.GetPosts]
    respond $ responseOk $ encodePretty posts
  | otherwise = respond $ responseNotFound "Unknown method called"

  where queryItems = queryString req
        query' = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req
        headers = requestHeaders req
        base64LoginAndPassword = snd $ head $ filter (\x -> fst x == "Authorization") headers