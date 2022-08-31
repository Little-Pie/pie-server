{-# LANGUAGE OverloadedStrings #-}

module Endpoints.RemoveAdmin where

import qualified Types.API.Id as API
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

removeAdmin :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
removeAdmin conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let id' = API.id bodyParsed
    users <- query conn "select * from users where id=(?)" (Only id') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if not $ isAdmin x
      then pure "This user is already not admin"
      else do
        execute conn "UPDATE users SET is_admin = (?) WHERE id = (?)" $ (False,id')
        pure "Now this user is not an admin"