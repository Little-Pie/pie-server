{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers where

import Types.Entities.User (User(..))
import DbQuery.User (getUserByLogin)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Aeson (Value(..), FromJSON, decode, decodeStrict, parseJSON, (.:))
import qualified Data.ByteString.Base64 as BASE
import Network.HTTP.Types (Status, Query, statusCode, status200, hContentType, status500, notFound404, badRequest400, unauthorized401)
import Network.Wai (Middleware, Response, responseStatus, responseLBS, rawPathInfo, rawQueryString)
import Database.PostgreSQL.Simple as PSQL (ConnectInfo(..), Only(..), Connection, defaultConnectInfo)

data Config = Config {limit :: Int
                     ,offset :: Int
                     ,connectHost :: String
                     ,connectDatabase :: String
                     ,connectUser :: String
                     ,connectPassword :: String
                     }

instance FromJSON Config where
  parseJSON (Object config) = Config <$>
                              config .: "limit" <*>
                              config .: "offset" <*>
                              config .: "connectHost" <*>
                              config .: "connectDatabase" <*>
                              config .: "connectUser" <*>
                              config .: "connectPassword"

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- BS.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf

localPG :: Config -> ConnectInfo
localPG Config {..} = defaultConnectInfo
        { PSQL.connectHost = connectHost
        , PSQL.connectDatabase = connectDatabase
        , PSQL.connectUser = connectUser
        , PSQL.connectPassword = connectPassword
        }

authorize :: Connection -> Maybe BS.ByteString -> IO (LBS.ByteString,Maybe User)
authorize conn mbBase64LoginAndPassword = case mbBase64LoginAndPassword of
  Nothing -> pure ("Found no header for Authorization",Nothing)
  Just base64LoginAndPassword -> case BASE.decode base64LoginAndPassword of
    Left err -> pure (LBSC.pack err,Nothing)
    Right loginPassword -> do
      let login' = BS.takeWhile (/= ':') loginPassword
      users <- getUserByLogin conn (BS.unpack login')
      case users of
        [] -> pure ("Wrong login",Nothing)
        [user] -> do
          let loginPassword' = BS.pack $ login user ++ ":" ++ password user
          if loginPassword == loginPassword'
          then pure ("User is authorized",Just user)
          else pure ("Wrong password",Nothing)

getQueryFilters :: [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
getQueryFilters queryItems = foldl (\acc n -> case lookup' n queryItems of
                                                Nothing -> acc
                                                Just filterParam -> (n,filterParam):acc) [] filters

filters :: [BS.ByteString]
filters = ["createdAt","createdUntil","createdSince","author","categoryId","title","text"] 

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

responseImage :: BS.ByteString -> LBS.ByteString -> Response
responseImage contentType = responseLBS status200 [(hContentType,contentType)]

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

withParsedRequest :: FromJSON a => LBS.ByteString -> (a -> IO Response) -> IO Response
withParsedRequest reqBody f =
  case decode reqBody of
    Nothing -> pure $ responseBadRequest "Couldn't parse body"
    Just parsedReq -> f parsedReq

withAuthorization :: Connection -> Maybe BS.ByteString -> (User -> IO Response) -> IO Response
withAuthorization conn mbBase64LoginAndPassword f = do
  (str, mbUser) <- authorize conn mbBase64LoginAndPassword
  case mbUser of
    Nothing -> pure $ responseUnauthorized str
    Just authorizedUser -> f authorizedUser
