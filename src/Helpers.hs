{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers where

import Config (Config (..), Environment (..))
import Data.Aeson (FromJSON, Value (..), decode, decodeStrict, parseJSON, (.:))
import qualified Data.ByteString.Base64 as BASE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Database.PostgreSQL.Simple as PSQL (ConnectInfo (..), Connection, Only (..), defaultConnectInfo, execute_)
import DbQuery.User (getUserByLogin)
import Hash (makeStringHash)
import Logging (LoggingLevel (..))
import Network.HTTP.Types (Query, Status, badRequest400, hContentType, notFound404, status200, status500, statusCode, unauthorized401)
import Network.Wai (Middleware, Response, rawPathInfo, rawQueryString, responseLBS, responseStatus)
import System.IO (Handle, hFlush, hPutStrLn)
import Types.Entities.User (User (..))

printDebug :: Environment -> String -> IO ()
printDebug Environment {..} str =
  if loggingLevel < Release
    then printLog logHandle str
    else pure ()

printRelease :: Environment -> String -> IO ()
printRelease Environment {..} str =
  if loggingLevel < Warning
    then printLog logHandle str
    else pure ()

printWarning :: Environment -> String -> IO ()
printWarning Environment {..} str =
  if loggingLevel < Error
    then printLog logHandle str
    else pure ()

printError :: Environment -> String -> IO ()
printError Environment {..} = printLog logHandle

printLog :: Handle -> String -> IO ()
printLog logHandle str = do
  hPutStrLn logHandle str
  hFlush logHandle

localPG :: Config -> ConnectInfo
localPG Config {..} =
  defaultConnectInfo
    { PSQL.connectHost = connectHost,
      PSQL.connectDatabase = connectDatabase,
      PSQL.connectUser = connectUser,
      PSQL.connectPassword = connectPassword
    }

authorize :: Connection -> Maybe BS.ByteString -> IO (LBS.ByteString, Maybe User)
authorize conn mbBase64LoginAndPassword = case mbBase64LoginAndPassword of
  Nothing -> pure ("Found no header for Authorization", Nothing)
  Just base64LoginAndPassword -> case BASE.decode base64LoginAndPassword of
    Left err -> pure (LBSC.pack err, Nothing)
    Right loginPassword -> do
      let login' = BS.takeWhile (/= ':') loginPassword
      let password' = BS.drop 1 $ BS.dropWhile (/= ':') loginPassword
      users <- getUserByLogin conn (BS.unpack login')
      case users of
        [] -> pure ("Wrong login", Nothing)
        [user] -> do
          let loginPassword' = BS.pack $ login user ++ ":" ++ password user
          if login' <> ":" <> BS.pack (makeStringHash $ BS.unpack password') == loginPassword'
            then pure ("User is authorized", Just user)
            else pure ("Wrong password", Nothing)

getQueryFilters :: [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
getQueryFilters queryItems =
  foldl
    ( \acc n -> case lookup' n queryItems of
        Nothing -> acc
        Just filterParam -> (n, filterParam) : acc
    )
    []
    filters

filters :: [BS.ByteString]
filters = ["createdAt", "createdUntil", "createdSince", "author", "categoryId", "title", "text"]

lookup' :: BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Maybe BS.ByteString
lookup' key' [] = Nothing
lookup' key' ((key, value) : items)
  | key' == key = value
  | otherwise = lookup' key' items

responseOk,
  responseNotFound,
  responseBadRequest,
  responseUnauthorized,
  responseInternalError ::
    LBS.ByteString -> Response
responseOk = responsePlainText status200
responseInternalError = responsePlainText status500
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400
responseUnauthorized = responsePlainText unauthorized401

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText =
  (`responseLBS` [(hContentType, "text/plain")])

responseImage :: BS.ByteString -> LBS.ByteString -> Response
responseImage contentType = responseLBS status200 [(hContentType, contentType)]

withLogging :: Middleware
withLogging app req respond =
  app req $ \response -> do
    putStrLn $ statusOf response ++ ": " ++ query
    respond response
  where
    query =
      BS.unpack $
        BS.concat
          [ rawPathInfo req,
            rawQueryString req
          ]
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
