{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import DbQuery.User
import qualified Types.API.CreateUser as CreateUser
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Network.Wai (Response)
import Network.HTTP.Types (Query)
import Database.PostgreSQL.Simple
import Helpers

createUser :: Connection -> LBS.ByteString -> IO Response
createUser conn body = case decode body :: Maybe CreateUser.CreateUserRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    let name' = CreateUser.name bodyParsed
    let login' = CreateUser.login bodyParsed
    let password' = CreateUser.password bodyParsed
    insertNewUser conn name' login' password'
    pure $ responseOk "User is created"