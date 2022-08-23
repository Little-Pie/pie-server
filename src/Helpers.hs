{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (toQuery,statusCode,methodGet,methodPost,status200,hContentType,Status,notFound404,badRequest400,QueryItem,Query)
import Network.Wai (lazyRequestBody,Middleware, responseStatus,responseLBS,Application,rawPathInfo,rawQueryString,requestMethod,Response,queryString)
import Database.PostgreSQL.Simple (ConnectInfo(..),defaultConnectInfo)

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "postgres"
        , connectUser = "postgres"
        , connectPassword = "5368"
        }

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