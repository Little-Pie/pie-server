{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers where

import Config (App, Config (..), Environment (..))
import Control.Monad.Reader (ask, liftIO, runReaderT)
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
import Network.Wai (Request, Response, ResponseReceived, rawPathInfo, rawQueryString, responseLBS, responseStatus)
import System.IO (Handle, hFlush, hPutStrLn)
import Types.Entities.User (User (..))

printDebug :: String -> App ()
printDebug str = do
  Environment {..} <- ask
  if loggingLevel < Release
    then liftIO $ printLog logHandle str
    else pure ()

printRelease :: String -> App ()
printRelease str = do
  Environment {..} <- ask
  if loggingLevel < Warning
    then liftIO $ printLog logHandle str
    else pure ()

printWarning :: String -> App ()
printWarning str = do
  Environment {..} <- ask
  if loggingLevel < Error
    then liftIO $ printLog logHandle str
    else pure ()

printError :: String -> App ()
printError str = do
  Environment {..} <- ask
  liftIO $ printLog logHandle str

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
    LBS.ByteString -> App Response
responseOk str = do
  printRelease $ (BS.unpack . LBS.toStrict) str
  pure $ responsePlainText status200 str
responseInternalError str = do
  printError $ (BS.unpack . LBS.toStrict) str
  pure $ responsePlainText status500 str
responseNotFound str = do
  printWarning $ (BS.unpack . LBS.toStrict) str
  pure $ responsePlainText notFound404 str
responseBadRequest str = do
  printWarning $ (BS.unpack . LBS.toStrict) str
  pure $ responsePlainText badRequest400 str
responseUnauthorized str = do
  printWarning $ BS.unpack . LBS.toStrict $ str
  pure $ responsePlainText unauthorized401 str

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText =
  (`responseLBS` [(hContentType, "text/plain")])

responseImage :: BS.ByteString -> LBS.ByteString -> App Response
responseImage contentType str = do
  printRelease "Responded with an image"
  pure $ responseLBS status200 [(hContentType, contentType)] str

withLogging :: (Request -> (Response -> IO ResponseReceived) -> App ResponseReceived) -> Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
withLogging app req respond = do
  env <- ask
  app req $ \response -> do
    runReaderT (printRelease $ statusOf response ++ ": " ++ query) env
    respond response
  where
    query =
      BS.unpack $
        BS.concat
          [ rawPathInfo req,
            rawQueryString req
          ]
    statusOf = show . statusCode . responseStatus

withParsedRequest :: FromJSON a => LBS.ByteString -> (a -> App Response) -> App Response
withParsedRequest reqBody f =
  case decode reqBody of
    Nothing -> responseBadRequest "Couldn't parse body"
    Just parsedReq -> f parsedReq

withAuthorization :: Maybe BS.ByteString -> (User -> App Response) -> App Response
withAuthorization mbBase64LoginAndPassword f = do
  Environment {..} <- ask
  (str, mbUser) <- liftIO $ authorize conn mbBase64LoginAndPassword
  case mbUser of
    Nothing -> responseUnauthorized str
    Just authorizedUser -> f authorizedUser
