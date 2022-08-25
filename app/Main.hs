{-# LANGUAGE OverloadedStrings #-}

module Main where

import Helpers
import Types.Entities.Post
import Types.Entities.User
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
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> do
      putStrLn "Serving..."
      run 4000 $ withLogging $ application config

--сортировка,категории,хэширование,логирование

application :: Config -> Application
application config req respond
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
      _ -> respond $ responseNotFound "Unknown method called"
  | path == "" = respond $
              if query' /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi GET!"
  | path == "users" = case lookup' "limit" queryItems of
    Nothing -> case lookup' "offset" queryItems of
      Nothing -> do
        users <- getLimitedUsers localPG (limit config) (offset config)
        case map name users of
          [""] -> respond $ responseNotFound "There are no users"
          _  -> respond $ responseOk $ encodePretty users
      Just offset ->
        case readMaybe (BS.unpack offset) :: Maybe Int of
          Nothing -> respond $ responseBadRequest "Offset should be a number"
          Just offset' -> do
            users <- getLimitedUsers localPG (limit config) offset'
            case map name users of
              [""] -> respond $ responseNotFound "There are no users"
              _  -> respond $ responseOk $ encodePretty users
    Just lim -> case readMaybe (BS.unpack lim) :: Maybe Int of
      Nothing -> respond $ responseBadRequest "Limit shold be a number"
      Just lim' -> do
        let limit' = if lim' > limit config || lim' < 0 then limit config else lim'
        case lookup' "offset" queryItems of
          Nothing -> do
            users <- getLimitedUsers localPG limit' (offset config)
            case map name users of
              [""] -> respond $ responseNotFound "There are no users"
              _  -> do
                respond $ responseOk $ encodePretty users
          Just offset -> case readMaybe (BS.unpack offset) :: Maybe Int of
            Nothing -> respond $ responseBadRequest "Offset should be a number"
            Just offset' -> do
              users <- getLimitedUsers localPG limit'offset'
              case map name users of
                [""] -> respond $ responseNotFound "There are no users"
                _  -> respond $ responseOk $ encodePretty users
  | path == "posts" = case lookup' "limit" queryItems of
    Nothing -> case lookup' "offset" queryItems of
      Nothing -> do
        posts <- getLimitedPosts localPG (limit config) (offset config)
        case map title posts of
          [""] -> respond $ responseNotFound "There are no posts"
          _  -> respond $ responseOk $ encodePretty posts
      Just offset ->
        case readMaybe (BS.unpack offset) :: Maybe Int of
          Nothing -> respond $ responseBadRequest "Offset should be a number"
          Just offset' -> do
            posts <- getLimitedPosts localPG (limit config) offset'
            case map title posts of
              [""] -> respond $ responseNotFound "There are no posts"
              _  -> respond $ responseOk $ encodePretty posts
    Just lim -> case readMaybe (BS.unpack lim) :: Maybe Int of
      Nothing -> respond $ responseBadRequest "Limit shold be a number"
      Just lim' -> do
        let limit' = if lim' > limit config || lim' < 0 then limit config else lim'
        case lookup' "offset" queryItems of
          Nothing -> do
            posts <- getLimitedPosts localPG limit' (offset config)
            case map title posts of
              [""] -> respond $ responseNotFound "There are no posts"
              _  -> do
                respond $ responseOk $ encodePretty posts
          Just offset -> case readMaybe (BS.unpack offset) :: Maybe Int of
            Nothing -> respond $ responseBadRequest "Offset should be a number"
            Just offset' -> do
              posts <- getLimitedPosts localPG limit' offset'
              case map title posts of
                [""] -> respond $ responseNotFound "There are no posts."
                _  -> respond $ responseOk $ encodePretty posts
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
  | path == "removeAuthor" = do
    conn <- connect localPG
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
                      answer <- removeAuthor conn userId'
                      LBSC.putStrLn answer
                      respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | path == "makeAdmin" = do
    conn <- connect localPG
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
                      answer <- makeAdmin conn userId'
                      LBSC.putStrLn answer
                      respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | path == "removeAdmin" = do
    conn <- connect localPG
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
                      answer <- removeAdmin conn userId'
                      LBSC.putStrLn answer
                      respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | path == "publishPost" = do
    conn <- connect localPG
    (str, mbId) <- authorize conn base64LoginAndPassword
    case mbId of
      Nothing -> do
        LBSC.putStrLn str
        respond $ responseUnauthorized str
      Just id -> do
        admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
        case isAdmin $ head admin of
          True -> case lookup' "id" queryItems of
            Nothing -> respond $ responseBadRequest "Enter post id"
            Just postId -> do
              case readMaybe (BS.unpack postId) :: Maybe Int of
                Nothing -> respond $ responseBadRequest "Post id should be a number"
                Just postId' -> do
                  answer <- publishPost conn postId'
                  LBSC.putStrLn answer
                  respond $ responseOk answer
          False -> do
            case lookup' "id" queryItems of
              Nothing -> respond $ responseBadRequest "Enter post id"
              Just postId -> case readMaybe (BS.unpack postId) :: Maybe Int of
                Nothing -> respond $ responseBadRequest "Post id should be a number"
                Just postId' -> do
                  post <- query conn "select * from posts where id=(?)" (Only postId') :: IO [Post]
                  case id == authorId (head post) of
                    False -> respond $ responseNotFound "You don't have unpublished news with such id"
                    True -> do
                      answer <- publishPost conn postId'
                      LBSC.putStrLn answer
                      respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | path == "deletePost" = do
    conn <- connect localPG
    (str, mbId) <- authorize conn base64LoginAndPassword
    case mbId of
      Nothing -> do
        LBSC.putStrLn str
        respond $ responseUnauthorized str
      Just id -> do
        admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
        case isAdmin $ head admin of
          True -> case lookup' "id" queryItems of
            Nothing -> respond $ responseBadRequest "Enter post id"
            Just postId -> do
              case readMaybe (BS.unpack postId) :: Maybe Int of
                Nothing -> respond $ responseBadRequest "Post id should be a number"
                Just postId' -> do
                  answer <- deletePost conn postId'
                  LBSC.putStrLn answer
                  respond $ responseOk answer
          False -> do
            case lookup' "id" queryItems of
              Nothing -> respond $ responseBadRequest "Enter post id"
              Just postId -> case readMaybe (BS.unpack postId) :: Maybe Int of
                Nothing -> respond $ responseBadRequest "Post id should be a number"
                Just postId' -> do
                  post <- query conn "select * from posts where id=(?)" (Only postId') :: IO [Post]
                  case id == authorId (head post) of
                    False -> respond $ responseNotFound "You don't have news with such id"
                    True -> do
                      answer <- deletePost conn postId'
                      LBSC.putStrLn answer
                      respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | path == "deleteUser" = do
    conn <- connect localPG
    (str, mbId) <- authorize conn base64LoginAndPassword
    case mbId of
      Nothing -> do
        LBSC.putStrLn str
        respond $ responseUnauthorized str
      Just id -> do
        admin <- query conn "select * from users where id=(?)" (Only id) :: IO [User]
        case isAdmin $ head admin of
          True -> case lookup' "id" queryItems of
            Nothing -> respond $ responseBadRequest "Enter user id"
            Just userId -> do
              case readMaybe (BS.unpack userId) :: Maybe Int of
                Nothing -> respond $ responseBadRequest "User id should be a number"
                Just userId' -> do
                  answer <- deleteUser conn userId'
                  LBSC.putStrLn answer
                  respond $ responseOk answer
          False -> do
            case lookup' "id" queryItems of
              Nothing -> respond $ responseBadRequest "Enter user id"
              Just userId -> case readMaybe (BS.unpack userId) :: Maybe Int of
                Nothing -> respond $ responseBadRequest "User id should be a number"
                Just userId' -> if id /= userId'
                then respond $ responseNotFound $ str `mappend` "\n" `mappend` "You can not delete other users"
                else do
                  answer <- deleteUser conn userId'
                  LBSC.putStrLn answer
                  respond $ responseOk $ str `mappend` "\n" `mappend` answer
  | otherwise = respond $ responseNotFound "Unknown method called"

  where queryItems = queryString req
        query' = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req
        headers = requestHeaders req
        base64LoginAndPassword = snd $ head $ filter (\x -> fst x == "Authorization") headers