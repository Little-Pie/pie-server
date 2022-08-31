{-# LANGUAGE OverloadedStrings #-}

module Endpoints.DeleteUser where

import qualified Types.API.Id as API
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

deleteUser :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
deleteUser conn body userId' = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let id' = API.id bodyParsed
    admin <- query conn "select * from users where id=(?)" (Only userId') :: IO [User]
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          execute conn "DELETE FROM users WHERE id=(?)" $ (Only id')
          pure "User is deleted"
        False -> if id' /= userId'
          then pure "You can not delete other users"
          else do
            users <- query conn "select * from users where id=(?)" $ (Only userId') :: IO [User]
            case users of
              [] -> pure "There are no users with such id"
              _ -> do
                execute conn "DELETE FROM users WHERE id=(?)" $ (Only id')
                pure "User is deleted"