{-# LANGUAGE OverloadedStrings #-}

module Endpoints.CreateUser where

import qualified Types.API.CreateUser as CreateUser
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Network.HTTP.Types (Query)
import Database.PostgreSQL.Simple

createUser :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
createUser conn body = case decode body :: Maybe CreateUser.CreateUserRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let name' = CreateUser.name bodyParsed
    let login' = CreateUser.login bodyParsed
    let password' = CreateUser.password bodyParsed
    execute conn "INSERT INTO users (name,login,password,is_admin,is_author) VALUES (?,?,?,?,?)" $ (name',login',password',False,False)
    pure "User is created"