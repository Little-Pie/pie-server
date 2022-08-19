{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsing
import Types.API.EditUser
import Types.API.CreateUser
import Network.Wai.Handler.Warp (run)
import Network.Wai (lazyRequestBody,Middleware, responseStatus,responseLBS,Application,rawPathInfo,rawQueryString,requestMethod,Response,queryString)
import Network.HTTP.Types (statusCode,methodGet,methodPost,status200,hContentType,Status,notFound404,badRequest400,QueryItem,Query)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
--import Database.PostgreSQL.Simple.FromRow
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson
--import Data.Time.Clock (UTCTime,getCurrentTime)

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "postgres"
        , connectUser = "postgres"
        , connectPassword = "5368"
        }

createUser :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
createUser conn body = case decode body :: Maybe CreateUserRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let name = createName bodyParsed
    let login = createLogin bodyParsed
    let password = createPassword bodyParsed
    execute conn "INSERT INTO users (name,login,password,admin,posting_news) VALUES (?,?,?,?,?)" $ (name,login,password,False,False)
    pure "User is created"

editUser :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
editUser conn body = case decode body :: Maybe EditUserRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    userList <- query_ conn "select * from users"
    let user' = userList !! 1
    let newUser = user' {
      name = maybe (name user') id (editName bodyParsed),
      login = maybe (login user') id (editLogin bodyParsed),
      password = maybe (password user') id (editPassword bodyParsed)}
    execute conn "UPDATE users SET (name,login,password) = (?,?,?) WHERE id = 1" $ (name newUser,login newUser,password newUser)
    pure ("Changes applied")

showUsers :: Connection -> IO [User]
showUsers conn = query_ conn "select * from users"

main :: IO ()
main = do
  putStrLn "Serving..."
  run 4000 $ withLogging application

withLogging :: Middleware
withLogging app req respond =
  app req $ \response -> do
    putStrLn $ statusOf response ++ ": " ++ query
    respond response
  where
    query = BS.unpack
          $ BS.concat [ rawPathInfo    req
                      , rawQueryString req ]
    statusOf = show . statusCode . responseStatus

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
        body <- bodyIO
        putStrLn $ LBSC.unpack body
        conn <- connect localPG
        answer <- editUser conn body
        LBSC.putStrLn answer
        respond $ responseOk answer
      _ -> respond $ responseNotFound "Unknown method called"
  | path == "" = respond $
              if query /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi GET!"
  | path == "users" =
              if query /= ""
              then  respond $ responseBadRequest "No query parameters needed!"
              else do
                conn <- connect localPG
                putStrLn "Connected to database"
                userList <- showUsers conn
                case map name userList of
                  [""] -> respond $ responseNotFound "There are no users."
                  _  -> do
                    respond $ responseOk $ encodePretty $ UserList userList
  | otherwise = respond $ responseNotFound "Unknown method called"

  where queryItems = queryString req -- [QueryItems] = [(ByteString, Maybe ByteString)]
        query = rawQueryString req
        path = BS.tail $ rawPathInfo req
        bodyIO = lazyRequestBody req

lookup' :: BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Maybe BS.ByteString
lookup' key' [] = Nothing
lookup' key' ((key,value):items) | key' == key = value
                                 | otherwise = lookup' key' items

responseOk, responseNotFound, responseBadRequest
  :: LBS.ByteString -> Response
responseOk = responsePlainText status200
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText =
  (`responseLBS` [(hContentType,"text/plain")])