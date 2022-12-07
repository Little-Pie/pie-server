module Routing where

import Authorization (withAuthorization)
import Config (App)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Endpoints
  ( createCategory,
    createPost,
    createUser,
    editCategory,
    editPost,
    getCategories,
    getImageById,
    getPosts,
    getUsers,
  )
import Helpers
  ( lookup',
    responseBadRequest,
    responseNotFound,
    withParsedRequest,
  )
import Network.HTTP.Types
  ( RequestHeaders,
    methodGet,
    methodPost,
  )
import Network.Wai
  ( Request,
    Response,
    ResponseReceived,
    lazyRequestBody,
    queryString,
    rawPathInfo,
    requestHeaders,
    requestMethod,
  )
import Text.Read (readMaybe)

application :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
application req respond
  | requestMethod req == methodPost = do
    body <- liftIO bodyIO
    case path of
      "createUser" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createUser authorizedUser)
        liftIO $ respond response
      "createPost" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createPost authorizedUser)
        liftIO $ respond response
      "editPost" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (editPost authorizedUser)
        liftIO $ respond response
      "createCategory" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (createCategory authorizedUser)
        liftIO $ respond response
      "editCategory" -> do
        response <- withAuthorization mbBase64LoginAndPassword $ \authorizedUser ->
          withParsedRequest body (editCategory authorizedUser)
        liftIO $ respond response
      _ -> do
        response <- responseNotFound ""
        liftIO $ respond response
  | requestMethod req == methodGet = do
    case path of
      "getImageById" -> case lookup' "id" queryItems of
        Nothing -> do
          response <- responseBadRequest "Enter image id"
          liftIO $ respond response
        Just imageId -> case readMaybe (BS.unpack imageId) :: Maybe Int of
          Nothing -> do
            response <- responseBadRequest "Image id should be a number"
            liftIO $ respond response
          Just imageId' -> do
            response <- getImageById imageId'
            liftIO $ respond response
      "categories" -> do
        response <- getCategories queryItems
        liftIO $ respond response
      "users" -> do
        response <- getUsers queryItems
        liftIO $ respond response
      "posts" -> do
        response <- getPosts queryItems
        liftIO $ respond response
      _ -> do
        response <- responseNotFound ""
        liftIO $ respond response
  | otherwise = do
    response <- responseNotFound ""
    liftIO $ respond response
  where
    queryItems = queryString req
    path = BS.tail $ rawPathInfo req
    bodyIO = lazyRequestBody req
    headers = requestHeaders req
    mbBase64LoginAndPassword = getBase64LoginAndPassword headers

    getBase64LoginAndPassword :: RequestHeaders -> Maybe BS.ByteString
    getBase64LoginAndPassword someHeaders = case someHeaders of
      [] -> Nothing
      ((headerName, bStr) : xs) ->
        if headerName == "Authorization"
          then Just bStr
          else getBase64LoginAndPassword xs
