{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rooting
import Helpers
import Types.Entities.Post
import Types.Entities.User
import qualified Types.API.GetPosts as API
import Endpoints.CreateCategory
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
import Control.Monad
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai (requestHeaders,Application,lazyRequestBody,rawPathInfo,rawQueryString,requestMethod,queryString)
import Network.HTTP.Types (methodGet,methodPost,Header,RequestHeaders)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Data.Aeson.Encode.Pretty (encodePretty)
--import Data.Time.Clock (UTCTime,getCurrentTime)

main :: IO ()
main = do
  conn <- connect localPG
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> do
      putStrLn "Serving..."
      run 4000 $ withLogging $ application conn config
      close conn