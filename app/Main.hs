{-# LANGUAGE OverloadedStrings #-}

module Main where

import Helpers
import Types.Entities.Post
import Types.Entities.User
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
  | requestMethod req == methodPost = case path of
      "" -> respond $
              if query /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi POST!"
      "createUser" -> do
        body <- bodyIO
        putStrLn $ LBSC.unpack body
        conn <- connect localPG
        --time <- getCurrentTime
        --let timeStamp = LBSC.pack $ take 19 $ show time
        answer <- createUser conn body
        LBSC.putStrLn answer
        respond $ responseOk answer
      "editUser" -> do
        conn <- connect localPG
        (str, mbId) <- authorize conn base64LoginAndPassword
        case mbId of
          Nothing -> do
            LBSC.putStrLn str
            respond $ responseUnauthorized str
          Just id -> do
            --case lookup' "id" queryItems of
              --Nothing -> respond $ responseBadRequest "Enter user id"
              --Just userId -> do
                LBSC.putStrLn str
                body <- bodyIO
                putStrLn $ LBSC.unpack body
                answer <- editUser conn body id
                LBSC.putStrLn answer
                respond $ responseOk $ str `mappend` "\n" `mappend` answer
      "createPost" -> do
        body <- bodyIO
        putStrLn $ LBSC.unpack body
        conn <- connect localPG
        answer <- createPost conn body
        LBSC.putStrLn answer
        respond $ responseOk answer
      "publishPost" -> do
        case lookup' "id" queryItems of
          Nothing -> respond $ responseBadRequest "Enter post id"
          Just postId -> do
            body <- bodyIO
            putStrLn $ LBSC.unpack body
            conn <- connect localPG
            case readMaybe (BS.unpack postId) :: Maybe Int of
              Nothing -> respond $ responseBadRequest "Enter post id"
              Just postId' -> do
                answer <- publishPost conn postId'
                LBSC.putStrLn answer
                respond $ responseOk answer
      _ -> respond $ responseNotFound "Unknown method called"
  | path == "" = respond $
              if query /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi GET!"
  | path == "users" =
              if query /= ""
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
              if query /= ""
              then respond $ responseBadRequest "No query parameters needed!"
              else do
                conn <- connect localPG
                putStrLn "Connected to database"
                posts <- query_ conn "select * from posts order by id" :: IO [Post]
                case map title posts of
                  [""] -> respond $ responseNotFound "There are no posts."
                  _  -> do
                    respond $ responseOk $ encodePretty $ filter (\x -> isPublished x == True) posts
  | otherwise = respond $ responseNotFound "Unknown method called"

  where queryItems = queryString req
        query = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req
        headers = requestHeaders req
        base64LoginAndPassword = snd $ head $ filter (\x -> fst x == "Authorization") headers