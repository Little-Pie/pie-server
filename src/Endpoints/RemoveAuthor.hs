{-# LANGUAGE OverloadedStrings #-}

module Endpoints.RemoveAuthor where

import qualified Types.API.Id as API
import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

removeAuthor :: Connection -> LBS.ByteString -> IO (LBS.ByteString)
removeAuthor conn body = case decode body :: Maybe API.IdRequest of
  Nothing -> pure "Couldn't parse body"
  Just bodyParsed -> do
    let id' = API.id bodyParsed
    users <- query conn "select * from users where id=(?)" (Only id') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if not $ isAuthor x
      then pure "This user is already not author"
      else do
        execute conn "UPDATE users SET \"isAuthor\" = (?) WHERE id = (?)" $ (False,id')
        pure "Now this user is not an author"