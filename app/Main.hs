{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai (Middleware, responseStatus,responseLBS,Application,rawPathInfo,rawQueryString,requestMethod,Response)
import Network.HTTP.Types (statusCode,methodGet,status200,hContentType,Status,notFound404,badRequest400)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "postgres"
        , connectUser = "postgres"
        , connectPassword = "5368"
        }

createUser :: Connection -> BS.ByteString -> IO ()
createUser conn name = do
  execute conn "INSERT INTO users (name) VALUES (?)" $ (Only name)
  pure ()

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
  | requestMethod req /= methodGet = 
    respond $ responseBadRequest "Only GET method is allowed!"
  | path == "" = respond $
              if query /= ""
              then responseBadRequest "No query parameters needed!"
              else responseOk "Hi!"
  | path == "createUser" =
              if query == ""
              then respond $ responseBadRequest "User name is needed!"
              else do
                conn <- connect localPG
                putStrLn "Connected to database"
                createUser conn (BS.tail query)
                respond $ responseOk "User is created"
  | otherwise = respond $ responseNotFound "Unknown method called"

  where query = rawQueryString req
        path = BS.tail $ rawPathInfo req

responseOk, responseNotFound, responseBadRequest
  :: LBS.ByteString -> Response
responseOk = responsePlainText status200
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText =
  (`responseLBS` [(hContentType,"text/plain")])