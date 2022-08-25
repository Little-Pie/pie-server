{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Types.Entities.User
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Aeson
import qualified Data.ByteString.Base64 as BASE
import Network.HTTP.Types (toQuery,statusCode,methodGet,methodPost,status200,hContentType,Status,status500,notFound404,badRequest400,unauthorized401,QueryItem,Query)
import Network.Wai (lazyRequestBody,Middleware, responseStatus,responseLBS,Application,rawPathInfo,rawQueryString,requestMethod,Response,queryString)
import Database.PostgreSQL.Simple (Only(..),Connection,query,query_,ConnectInfo(..),defaultConnectInfo)

data Config = Config {limit :: Int}

instance FromJSON Config where
  parseJSON (Object config) = Config <$> config .: "limit"  

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- BS.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "postgres"
        , connectUser = "postgres"
        , connectPassword = "5368"
        }

authorize :: Connection -> BS.ByteString -> IO (LBS.ByteString,Maybe Int)
authorize conn base64LoginAndPassword = case BASE.decode base64LoginAndPassword of
  Left err -> pure (LBSC.pack err,Nothing)
  Right loginPassword -> do
    let login' = BS.takeWhile (/= ':') loginPassword
    users <- query conn "select * from users where login=(?)" (Only $ BS.unpack login') :: IO [User]
    case users of
      [] -> pure ("Wrong login",Nothing)
      [us] -> do
        let loginPassword' = BS.pack $ login us ++ ":" ++ password us
        if loginPassword == loginPassword'
        then pure ("User is authorized",Just (userId us))
        else pure ("Wrong password",Nothing)

lookup' :: BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Maybe BS.ByteString
lookup' key' [] = Nothing
lookup' key' ((key,value):items) | key' == key = value
                                 | otherwise = lookup' key' items

responseOk, responseNotFound, responseBadRequest, responseUnauthorized, responseInternalError
  :: LBS.ByteString -> Response
responseOk = responsePlainText status200
responseInternalError = responsePlainText status500
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400
responseUnauthorized = responsePlainText unauthorized401

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