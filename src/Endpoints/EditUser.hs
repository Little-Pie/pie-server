{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditUser where

import qualified DbQuery.User as DB
import Types.Entities.User
import qualified Types.API.EditUser as API
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.Wai (Response)
import Helpers

editUser :: Connection -> LBS.ByteString -> Int -> IO (Response)
editUser conn body authorizedUserId = case decode body :: Maybe API.EditUserRequest of
  Nothing -> pure $ responseBadRequest "Couldn't parse body"
  Just bodyParsed -> do
    case API.userId bodyParsed of
      Nothing -> do
        userDb <- DB.getUserById conn authorizedUserId
        case userDb of
          [] -> pure $ responseBadRequest "There are no users with such id"
          (user:_) -> do
            let newUser = user {
              name = maybe (name user) id (API.name bodyParsed),
              login = maybe (login user) id (API.login bodyParsed),
              password = maybe (password user) id (API.password bodyParsed)}
            DB.editUser conn (name newUser) (login newUser) (password newUser) authorizedUserId
            pure $ responseOk "Changes applied"
      Just editUserId -> do
        admin <- DB.getUserById conn authorizedUserId
        case admin of
          [] -> pure $ responseInternalError "Something went wrong: empty list"
          (x:_) -> if isAdmin x
            then do
              userDb <- DB.getUserById conn editUserId
              case userDb of
                [] -> pure $ responseInternalError "Something went wrong: empty list"
                (user:_) -> do
                  let newUser = user {
                    name = maybe (name user) id (API.name bodyParsed),
                    login = maybe (login user) id (API.login bodyParsed),
                    password = maybe (password user) id (API.password bodyParsed)}
                  DB.editUser conn (name newUser) (login newUser) (password newUser) editUserId
                  pure $ responseOk "Changes applied"
            else pure $ responseNotFound ""