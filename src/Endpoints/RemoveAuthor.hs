{-# LANGUAGE OverloadedStrings #-}

module Endpoints.RemoveAuthor where

import Types.Entities.User
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Database.PostgreSQL.Simple

removeAuthor :: Connection -> Int -> IO (LBS.ByteString)
removeAuthor conn userId' = do
    users <- query conn "select * from users where id=(?)" (Only userId') :: IO [User]
    case users of
      [] -> pure "There are no users with such id"
      [x] -> if not $ isAuthor x
      then pure "This user is already not author"
      else do
        execute conn "UPDATE users SET is_author = (?) WHERE id = (?)" $ (False,userId')
        pure "Now this user is not an author"