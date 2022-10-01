{-# LANGUAGE OverloadedStrings #-}

module Endpoints.DeleteUser where

import qualified DbQuery.User as DB
import qualified Types.API.Id as API
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

deleteUser :: Connection -> LBS.ByteString -> Int -> IO (LBS.ByteString)
deleteUser conn body authorizedUserId = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let removeUserId = API.id bodyParsed
    admin <- DB.getUserById conn authorizedUserId
    case admin of
      [] -> pure "Something went wrong: empty list"
      [x] -> case isAdmin x of
        True -> do
          DB.deleteUser conn removeUserId
        False -> if removeUserId /= authorizedUserId
          then pure "You can not delete other users"
          else do
            users <- DB.getUserById conn authorizedUserId
            case users of
              [] -> pure "There are no users with such id"
              _ -> do
                DB.deleteUser conn removeUserId