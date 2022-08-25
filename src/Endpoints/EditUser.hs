{-# LANGUAGE OverloadedStrings #-}

module Endpoints.EditUser where

import Types.Entities.User
import qualified Types.API.EditUser as API
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

editUser :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
editUser conn body userId' = case decode body :: Maybe API.EditUserRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    userList <- query_ conn "select * from users order by id"
    if userId' `notElem` (map userId userList)
    then pure "There is no users with such id"
    else do
      user' <- query conn "select * from users where id=(?)" $ (Only userId') :: IO [User]
      let user = head user'
      let newUser = user {
        name = maybe (name user) id (API.name bodyParsed),
        login = maybe (login user) id (API.login bodyParsed),
        password = maybe (password user) id (API.password bodyParsed)}
      execute conn "UPDATE users SET (name,login,password) = (?,?,?) WHERE id = (?)" $ (name newUser,login newUser,password newUser,userId')
      pure ("Changes applied")