{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import Types.Entities.User
import DbQuery.User
import qualified Types.API.CreateUser as API
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Network.Wai (Response)
import Database.PostgreSQL.Simple
import Helpers

createUser :: Connection -> LBS.ByteString -> Int -> IO Response
createUser conn body userId = case decode body :: Maybe API.CreateUserRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    admin <- getUserById conn userId
    case admin of
      [] -> pure $ responseInternalError "Something went wrong: empty list"
      (x:_) -> if isAdmin x
        then do
          let name' = API.name bodyParsed
          let login' = API.login bodyParsed
          let password' = API.password bodyParsed
          let isAdmin' = API.isAdmin bodyParsed
          let isAuthor' = API.isAuthor bodyParsed
          insertNewUser conn name' login' password' isAdmin' isAuthor'
          pure $ responseOk "User is created"
        else pure $ responseNotFound ""