{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditUser where

import Types.Entities.User
import qualified Types.API.EditUser as API
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Network.HTTP.Types (Query)
import Database.PostgreSQL.Simple

editUser :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
editUser conn body userId = case decode body :: Maybe API.EditUserRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    userList <- query_ conn "select * from users order by id"
    if userId >= length userList || userId < 1
    then pure "There is no users with such id"
    else do
      let user = userList !! userId
      let newUser = user {
        name = maybe (name user) id (API.name bodyParsed),
        login = maybe (login user) id (API.login bodyParsed),
        password = maybe (password user) id (API.password bodyParsed)}
      execute conn "UPDATE users SET (name,login,password) = (?,?,?) WHERE id = (?)" $ (name newUser,login newUser,password newUser,userId)
      pure ("Changes applied")