{-# LANGUAGE OverloadedStrings #-}

module Main where

import Helpers
import Types.Entities.Post
import Types.Entities.User
import Endpoints.MakeAuthor
import Endpoints.CreateUser
import Endpoints.EditUser
import Endpoints.CreatePost
import Endpoints.PublishPost
import Text.Read (readMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai (requestHeaders,Application,lazyRequestBody,rawPathInfo,rawQueryString,requestMethod,queryString)
import Network.HTTP.Types (methodGet,methodPost,Header,RequestHeaders)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.Time.Clock (UTCTime,getCurrentTime)

main :: IO ()
main = do
  putStrLn "Serving..."
  run 4000 $ withLogging application

application :: Application
application req respond
  | requestMethod req /= methodPost && requestMethod req /= methodGet = respond $ responseBadRequest "Use method GET or POST"
  | requestMethod req == methodPost = do
    body <- bodyIO
    conn <- connect localPG
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
                      [] -> respond $ responseInternalError "Something went wrong"
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
      "publishPost" -> do
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
            case isAdmin $ head admin of
              True -> do
                case lookup' "id" queryItems of
                  Nothing -> respond $ responseBadRequest "Enter post id"
                  Just postId -> do
                    putStrLn $ LBSC.unpack body
                    case readMaybe (BS.unpack postId) :: Maybe Int of
                      Nothing -> respond $ responseBadRequest "Post id should be a number"
                      Just postId' -> do
                        answer <- publishPost conn postId'
                        LBSC.putStrLn answer
                        respond $ responseOk answer
              False -> do
                case lookup' "id" queryItems of
                  Nothing -> respond $ responseBadRequest "Enter post id"
                  Just postId -> do
                    putStrLn $ LBSC.unpack body
                    case readMaybe (BS.unpack postId) :: Maybe Int of
                      Nothing -> respond $ responseBadRequest "Post id should be a number"
                      Just postId' -> do
                        post <- query conn "select * from posts where id=(?)" (Only postId') :: IO [Post]
                        case id == authorId (head post) of
                          False -> respond $ responseNotFound "You don't have unpublished news with such id"
                          True -> do
                            answer <- publishPost conn postId'
                            LBSC.putStrLn answer
                            respond $ responseOk $ str `mappend` "\n" `mappend` answer 
      _ -> respond $ responseNotFound "Unknown method called"
  | path == "" = respond $
              if query' /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi GET!"
  | path == "users" =
              if query' /= ""
              then respond $ responseBadRequest "No query parameters needed!"
              else do
                conn <- connect localPG
                putStrLn "Connected to database"
                userList <- query_ conn "select * from users order by id" :: IO [User]
                case map name userList of
                  [""] -> respond $ responseNotFound "There are no users."
                  _  -> do
                    respond $ responseOk $ encodePretty userList
  | path == "posts" =
              if query' /= ""
              then respond $ responseBadRequest "No query parameters needed!"
              else do
                conn <- connect localPG
                putStrLn "Connected to database"
                posts <- query_ conn "select * from posts order by id" :: IO [Post]
                case map title posts of
                  [""] -> respond $ responseNotFound "There are no posts."
                  _  -> do
                    respond $ responseOk $ encodePretty $ filter (\x -> isPublished x == True) posts
  | path == "makeAuthor" = do
    conn <- connect localPG
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
                      answer <- makeAuthor conn userId'
                      LBSC.putStrLn answer
                      respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | otherwise = respond $ responseNotFound "Unknown method called"

  where queryItems = queryString req
        query' = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req
        headers = requestHeaders req
        base64LoginAndPassword = snd $ head $ filter (\x -> fst x == "Authorization") headers