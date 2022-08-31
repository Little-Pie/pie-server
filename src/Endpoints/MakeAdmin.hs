{-# LANGUAGE OverloadedStrings #-}

module Endpoints.MakeAdmin where

import qualified Types.API.Id as API
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

makeAdmin :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
makeAdmin conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let id' = API.id bodyParsed
    users <- query conn "select * from users where id=(?)" (Only id') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if isAdmin x
      then pure "This user is already admin"
      else do
        execute conn "UPDATE users SET is_admin = (?) WHERE id = (?)" $ (True,id')
        pure "Now this user is an admin"