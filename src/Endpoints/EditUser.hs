{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditUser where

import qualified DbQuery.User as DB
import Types.Entities.User
import qualified Types.API.EditUser as API
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

editUser :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
editUser conn body authorizedUserId = case decode body :: Maybe API.EditUserRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    userList <- DB.getUsers conn
    if authorizedUserId `notElem` (map userId userList)
    then pure "There is no users with such id"
    else do
      user' <- DB.getUserById conn authorizedUserId
      let user = head user'
      let newUser = user {
        name = maybe (name user) id (API.name bodyParsed),
        login = maybe (login user) id (API.login bodyParsed),
        password = maybe (password user) id (API.password bodyParsed)}
      DB.editUser conn (name newUser) (login newUser) (password newUser) authorizedUserId